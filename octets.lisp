;;;; octets.lisp -- substrate for encoding functionality

(cl:in-package :binascii)

(defun case-fold-decode-table (decode-table encode-table)
  (loop with table = (copy-seq decode-table)
     for c across encode-table
     do (setf (aref table (char-code (char-downcase c)))
              (aref table (char-code c)))
     finally (return table)))

(defun canonicalize-element-type (element-type &optional (errorp t))
  (cond
    ((eq element-type 'character) element-type)
    ((eq element-type 'base-char) element-type)
    ;; We want (UNSIGNED-BYTE 8), but there are a variety of
    ;; ways to express that and we don't want to go through
    ;; SUBTYPEP all the time.  Do a quick check for the most
    ;; likely form, then use SUBTYPEP for people who do things
    ;; weirdly.
    ((or (equal element-type '(unsigned-byte 8))
         (and (subtypep element-type '(unsigned-byte 8))
              (subtypep '(unsigned-byte 8) element-type)))
     'octet)
    (t
     (when errorp
       (error "Unsupported element-type ~A" element-type)))))

(declaim (inline array-data-and-offsets))
(defun array-data-and-offsets (v start end)
  "Like ARRAY-DISPLACEMENT, only more useful."
  #+sbcl
  (sb-kernel:with-array-data ((v v) (start start) (end end))
    (values v start end))
  #+cmu
  (lisp::with-array-data ((v v) (start start) (end end))
    (values v start end))
  #-(or sbcl cmu)
  (values v start (or end (length v))))

(defun encode-to-fresh-vector (octets state start end element-type)
  (declare (type encode-state state))
  (multiple-value-bind (input start end)
      (array-data-and-offsets octets start end)
    (let* ((fd (state-descriptor state))
           (length (funcall (fd-encoded-length fd) (- end start))))
      (declare (type format-descriptor fd))
      (flet ((frob (etype encode-fun)
               (let ((v (make-array length :element-type etype)))
                 (funcall encode-fun state v input
                          0 length start end t)
                 v)))
        (declare (inline frob))
        (ecase (canonicalize-element-type element-type)
          (character
           (frob 'character (fd-octets->string fd)))
          (base-char
           (frob 'base-char (fd-octets->string fd)))
          (octet
           (frob '(unsigned-byte 8) (fd-octets->octets/encode fd))))))))

(defun encode (octets format &key (start 0) end (element-type 'base-char))
  (encode-to-fresh-vector octets (find-encoder format) start end element-type))

(defun encode-octets (destination octets format &key (start 0) end
                      (output-start 0) output-end (element-type 'base-char)
                      finishp)
  "Encode OCTETS between START and END into ASCII characters
according to FORMAT and write them to DESTINATION according to ELEMENT-TYPE.

If DESTINATION is NIL and ELEMENT-TYPE is a subtype of CHARACTER, then a
string is returned.  If DESTINATION is NIL and ELEMENT-TYPE is
\(UNSIGNED-BYTE 8) or an equivalent type, then an octet vector is returned.

If ELEMENT-TYPE is a subtype of CHARACTER, then DESTINATION may also be
a string.  Similarly, if ELEMENT-TYPE is (UNSIGNED-BYTE 8) or an
equivalent type, then DESTINATION may be an octet vector.  In this case,
OUTPUT-START and OUTPUT-END are used to determine the portion of
DESTINATION where the encoded output may be placed.

If DESTINATION is not NIL, The index of the first input element that was
not read and the index of the first output element that was not updated
are returned as multiple values.  respectively, written are returned as
multiple values.  ELEMENT-TYPE is ignored.

If FINISHP is true, then in addition to any encoding of OCTETS, also output
any necessary padding required by FORMAT."
  (let* ((state (find-encoder format))
         (fd (state-descriptor state)))
    (declare (type encode-state state))
    (declare (type format-descriptor fd))
    (flet ((frob (encode-fun)
             (multiple-value-bind (input input-start input-end)
                 (array-data-and-offsets octets start end)
               (multiple-value-bind (output output-start output-end)
                   (array-data-and-offsets destination output-start output-end)
                 (funcall encode-fun state
                          output input
                          output-start output-end
                          input-start input-end finishp)))))
      (declare (inline frob))
      (etypecase destination
        (null
         (encode-to-fresh-vector octets state start end element-type))
        (string
         (frob (fd-octets->string fd)))
        ((array (unsigned-byte 8) (*))
         (frob (fd-octets->octets/encode fd)))))))

(defun decode-to-fresh-vector (string state start end decoded-length)
  (declare (type decode-state state))
  (multiple-value-bind (input start end)
      (array-data-and-offsets string start end)
    (let* ((fd (state-descriptor state))
           (length (or decoded-length
                       (funcall (fd-decoded-length fd) (- end start)))))
      (declare (type format-descriptor fd))
      (flet ((frob (v decode-fun)
               (multiple-value-bind (input-index output-index)
                   (funcall decode-fun state v input 0 length start end t)
                 ;; FIXME: we should check to see if we actually
                 ;; consumed all the input.  If we didn't, then we need
                 ;; to reallocate V and continue decoding.  Even though
                 ;; we said LASTP=T.  Hmmm.
                 (declare (ignore input-index))
                 (if (= output-index length)
                     v
                     (subseq v 0 output-index)))))
        (let ((octets (make-array length :element-type '(unsigned-byte 8))))
          (etypecase string
            (simple-string
             (frob octets (fd-string->octets fd)))
            (simple-octet-vector
             (frob octets (fd-octets->octets/decode fd)))))))))

(defun decode (string format &key (start 0) end case-fold map01 decoded-length)
  (decode-to-fresh-vector string (find-decoder format case-fold map01)
                          start end decoded-length))

(defun decode-octets (destination string format &key (start 0) end
                      (output-start 0) output-end case-fold map01 finishp
                      decoded-length)
  "Decode the characters of STRING between START and END into octets
according to FORMAT.  DECODED-LENGTH indicates the number of decoded
octets to expect.  DESTINATION may be NIL."
  (let ((state (find-decoder format case-fold map01)))
    (declare (type decode-state state))
    (flet ((frob (decode-fun)
             (multiple-value-bind (input input-start input-end)
                 (array-data-and-offsets string start end)
               (multiple-value-bind (output output-start output-end)
                   (array-data-and-offsets destination output-start output-end)
                 (funcall decode-fun state
                          output input
                          output-start output-end
                          input-start input-end finishp)))))
      (declare (inline frob))
      (etypecase string
        (null
         (decode-to-fresh-vector string state start end decoded-length))
        (string
         (frob (fd-string->octets (state-descriptor state))))
        ((array (unsigned-byte 8) (*))
         (frob (fd-octets->octets/decode (state-descriptor state))))))))

(defconstant +dt-invalid+ -1)

(defun make-decode-table (encode-table)
  (loop with table = (make-array 256 :element-type 'fixnum
                                 :initial-element +dt-invalid+)
     for char across encode-table
     for i from 0
     do (setf (aref table (char-code char)) i)
     finally (return table)))

(deftype decode-table () '(simple-array fixnum (256)))

(declaim (inline dtref))
(defun dtref (table i)
  (declare (type decode-table table))
  (declare (type index i))
  ;; FIXME: statically handle CHAR-CODE-LIMIT <= 256
  (if (>= i 256)
      +dt-invalid+
      (aref table i)))

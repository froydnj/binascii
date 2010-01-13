;;;; octets.lisp -- substrate for encoding functionality

(cl:in-package :binascii)

(deftype index () '(mod #.array-dimension-limit))

(defgeneric encoding-tools (format)
  (:documentation "Return three values: the basic encoding function for
FORMAT, an encoded-length function for FORMAT, and an encoding table for
FORMAT.  The encoding table specifies ASCII characters for encoded
values and is typically a SIMPLE-BASE-STRING."))

(defgeneric decoding-tools (format &key case-fold map01)
  (:documentation "Return three values: the basic decoding function for
FORMAT, a decoded-length function for FORMAT, and the decoding table for
FORMAT.  CASE-FOLD is a generalized boolean indicating whether to
compare characters case-insensitively.  MAP01 should be either NIL,
#\\I, or #\\L; if MAP01 is not NIL, then its value indicates what
character #\\1 maps to.  If MAP01 is not NIL, then \\#0 maps to O.

CASE-FOLD and MAP01 are silently ignored if they do not apply to
FORMAT."))

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

(defun determine-encoding-writer (destination length element-type)
  (etypecase destination
    (null
     (flet ((do-encode (etype transform)
              (let ((v (make-array (the fixnum length) :element-type etype))
                    (i -1))
                (values #'(lambda (c) (setf (aref v (incf i))
                                            (funcall transform c)))
                        v))))
       (declare (inline do-encode))
       (cond
         ((eq element-type 'character)
          (do-encode 'character #'identity))
         ((eq element-type 'base-char)
          (do-encode 'base-char #'identity))
         (t
          (do-encode '(unsigned-byte 8) #'char-code)))))
    (stream
     (cond
       ((or (eq element-type 'character) (eq element-type 'base-char))
        (values #'(lambda (c) (write-char c destination)) nil))
       (t
        (values #'(lambda (c) (write-byte (char-code c) destination)) nil))))
    (string
     (unless (or (eq element-type 'character) (eq element-type 'base-char))
       (error "Cannot output to a string with ~A :ELEMENT-TYPE" element-type))
     (values #'(lambda (c) (vector-push-extend c destination)) nil))
    ((array (unsigned-byte 8) (*))
     (unless (eq element-type 'octet)
       (error "Cannot output to an octet vector with ~A :ELEMENT-TYPE" element-type))
     (values #'(lambda (c) (vector-push-extend (char-code c) destination)) nil))))

(defun decode-octets* (destination string decode-fun length-fun decode-table
                       start end decoded-length)
  (declare (type function decode-fun length-fun))
  (let* ((end (or end (length string)))
         ;; For better or worse, a provided decoded length from the user
         ;; always wins.  But LENGTH-FUN may do some additional
         ;; validation of its own, so we want to make sure to call it
         ;; too (the additional cost of doing so is negligible).
         (guessed-length (funcall length-fun (- end start)))
         (length (or decoded-length guessed-length)))
    (declare (type index length))
    (etypecase destination
      (null
       (let* ((octets (make-array length :element-type '(unsigned-byte 8)))
              (i -1)
              (actual-length 0))
         (declare (type index actual-length))
         (funcall decode-fun string start end length decode-table
                  #'(lambda (o)
                      (setf (aref octets (incf i)) o)
                      (incf actual-length)))
         (if (= actual-length length)
             octets
             ;; FIXME: if we wanted to dig into SBCL internals, there's
             ;; a less consy way: (SB-KERNEL:%SHRINK-VECTOR).
             (subseq octets 0 actual-length))))
      (stream
       (funcall decode-fun string start end length decode-table
                #'(lambda (o) (write-byte o destination)))
       nil)
      ((array (unsigned-byte 8) (*))
       (funcall decode-fun string start end length decode-table
                #'(lambda (o) (vector-push-extend o destination)))
       nil))))

(defun encode-octets (destination octets format
                      &key (start 0) end (element-type 'base-char)
                      &allow-other-keys)
  "Encode OCTETS between START and END into ASCII characters
according to FORMAT and written to DESTINATION according to ELEMENT-TYPE.

If DESTINATION is NIL and ELEMENT-TYPE is a subtype of CHARACTER, then a
string is returned.  If DESTINATION is NIL and ELEMENT-TYPE is 
\(UNSIGNED-BYTE 8) or an equivalent type, then an octet vector is returned.

If DESTINATION is a STREAM, then the result is written to DESTINATION
using WRITE-CHAR or WRITE-BYTE as chosen by ELEMENT-TYPE.

If ELEMENT-TYPE is a subtype of CHARACTER, then DESTINATION may also be
a string with a fill pointer.  The result is written to the string as if
by use of VECTOR-PUSH-EXTEND.  Similarly, if ELEMENT-TYPE
is (UNSIGNED-BYTE 8) or an equivalent type, then DESTINATION may be an
octet vector with a fill pointer."
  (multiple-value-bind (encode-fun length-fun table) (encoding-tools format)
    (let* ((end (or end (length octets)))
           (length (- end start))
           (canonical-element-type (canonicalize-element-type element-type)))
      (multiple-value-bind (writer return-value)
          (determine-encoding-writer destination (funcall length-fun length)
                                     canonical-element-type)
        (funcall encode-fun octets start end table writer)
        return-value))))

(defun required-argument ()
  (error "Required argument not provided"))

(defstruct (encode-state
             (:copier nil)
             (:constructor))
  (encoded-length (required-argument) :read-only t)
  (octets->string (required-argument) :read-only t)
  (octets->octets (required-argument) :read-only t))

(defstruct (decode-state
             (:copier nil)
             (:constructor))
  (decoded-length (required-argument) :read-only t)
  (string->octets (required-argument) :read-only t)
  (octets->octets (required-argument) :read-only t))

(defun decode-octets (destination string format
                      &key (start 0) end decoded-length case-fold map01
                      &allow-other-keys)
  "Decode the characters of STRING between START and END into octets
according to FORMAT.  DECODED-LENGTH indictes the number of decoded octets
to expect.  DESTINATION may be NIL, an octet vector with a fill-pointer,
or a stream.  DECODED-LENGTH does not need to be provided for all formats."
  (declare (ignorable decoded-length))
  (multiple-value-bind (decode length table)
      (decoding-tools format :case-fold case-fold :map01 map01)
    (decode-octets* destination string decode length table
                    start end decoded-length)))

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

;;; By controlling the inlinability of this function, we can specialize
;;; decode functions on simple arrays while going through full calls for
;;; non-simple arrays, thus providing a balance of
;;; performance/functionality/code size.
;;;
;;; Yes, it's a little ugly, but it's because INLINE/NOTINLINE doesn't
;;; work quite right on local functions in some implementations.
;;; Assuming the implementation supports INLINE/NOTINLINE correctly,
;;; this way should work.
(declaim (inline decode-dispatch-wrap))
(defun decode-dispatch-wrap (decode-fun transform)
  (funcall decode-fun transform))
(declaim (notinline decode-dispatch-wrap))

(declaim (inline decode-dispatch))
(defun decode-dispatch (v decode-fun)
  (etypecase v
     ;; Probably not worth optimizing for BASE-CHAR vs. CHARACTER.
     (simple-string
      (locally (declare (inline decode-dispatch-wrap))
        (decode-dispatch-wrap decode-fun #'char-code)))
     ((simple-array (unsigned-byte 8) (*))
      (locally (declare (inline decode-dispatch-wrap))
        (decode-dispatch-wrap decode-fun #'identity)))
     ;; We're not particularly worried about speed in these two cases.
     (string
      (locally (declare (notinline decode-dispatch-wrap))
        (decode-dispatch-wrap decode-fun #'char-code)))
     ((array (unsigned-byte 8) (*))
      (locally (declare (notinline decode-dispatch-wrap))
        (decode-dispatch-wrap decode-fun #'identity)))))

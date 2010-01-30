;;;; ascii85.lisp -- The ascii85 encoding, as used in PDF and btoa/atob.

(cl:in-package :binascii)

(defvar *ascii85-encode-table*
  #.(coerce "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstu" 'simple-base-string))

(defun ascii85-format-descriptor ()
  (let* ((cell (load-time-value (list nil)))
         (fd (car cell)))
    (if fd
        fd
        (setf (car cell)
              (make-format-descriptor #'encoded-length/ascii85
                                      #'octets->string/ascii85
                                      #'octets->octets/encode/ascii85
                                      #'decoded-length-ascii85
                                      #'string->octets/ascii85
                                      #'octets->octets/decode/ascii85)))))

(defstruct (ascii85-encode-state
             (:include encode-state)
             (:copier nil)
             (:constructor make-ascii85-encode-state
                           (&aux (descriptor (ascii85-format-descriptor)))))
  (bits 0 :type (unsigned-byte 32))
  (pending 0 :type (integer 0 4))
  (output-group (make-array 5 :element-type 'base-char)
                :read-only t :type (simple-array base-char (5)))
  (output-pending 0 :type (integer 0 5))
  (table *ascii85-encode-table* :read-only t
         :type (simple-array base-car (85))))

(defun encoded-length/ascii85 (count)
  "Return the number of characters required to encode COUNT octets in Ascii85."
  (multiple-value-bind (q r) (truncate count 4)
    (let ((complete (* q 5)))
      (if (zerop r)
          complete
          (+ complete r 1)))))

(declaim (inline ascii85-encode))
(defun ascii85-encoder (state output input
                        output-start output-end
                        input-start input-end lastp converter)
  (declare (type ascii85-encode-state state))
  (declare (type simple-octet-vector input))
  (declare (type index output-start output-end input-start input-end))
  (declare (type function converter))
  (let ((input-index input-start)
        (output-index output-start)
        (bits (ascii85-encode-state-bits state))
        (pending (ascii85-encode-state-pending))
        (output-group (ascii85-encode-state-output-group state))
        (output-pending (ascii85-encode-state-output-pending state)))
    (declare (type index input-index output-index))
    (declare (type (unsigned-byte 32) bits))
    (declare (type (integer 0 4) pending))
    (declare (type (integer 0 5) output-pending))
    (flet ((expand-for-output (bits output-group)
             (cond
               ((zerop bits)
                (setf (aref output-group 0) #\z)
                1)
               (t
                (loop for i from 45 downto 0
                   do (multiple-value-bind (b index) (truncate bits 85)
                        (setf bits b
                              (aref output-group i)
                              (code-char (+ #.(char-code #\!) index))))
                   finally (return 5))))))
      (tagbody
       PAD-CHECK
         (when (ascii85-encode-state-finished-input-p state)
           (go FLUSH-BITS))
       INPUT-CHECK
         (when (>= input-index input-end)
           (go DONE))
       DO-INPUT
         (when (< pending 4)
           (setf bits (ldb (byte 32 0)
                           (logior (ash (aref input input-index)
                                        (- 24 (* pending 8)))
                                   bits)))
           (incf input-index)
           (incf pending)
           (go INPUT-CHECK))
       EXPAND-FOR-OUTPUT
         (setf output-pending (expand-for-output bits output-group))
       OUTPUT-CHECK
         (when (>= output-index output-end)
           (go DONE))
       DO-OUTPUT
         (when (> output-pending 0)
           (setf (aref output output-index)
                 (funcall converter
                          (aref output-group (decf output-pending))))
           (incf output-index)
           (cond
             ((zerop output-pending)
              (setf bits 0)
              (setf pending 0)
              (go INPUT-CHECK))
             (t
              (go OUTPUT-CHECK))))
       DONE
         (unless lastp
           (go RESTORE-STATE))
         (setf (ascii85-encode-state-finished-input-p state) t)
         (setf output-pending (expand-for-output bits output-group)
               output-pending (1+ pending))
       FLUSH-BITS
         (when (zerop output-pending)
           (go RESTORE-STATE))
       FLUSH-OUTPUT-CHECK
         (when (>= output-index output-end)
           (go RESTORE-STATE))
       DO-FLUSH-OUTPUT
         (when (> output-pending 0)
           (setf (aref output output-index)
                 (funcall converter
                          (aref output-group (decf output-pending))))
           (incf output-index)
           (cond
             ((zerop output-pending)
              (setf bits 0)
              (setf pending 0)
              (go RESTORE-STATE))
             (t
              (go FLUSH-OUTPUT-CHECK))))
       RESTORE-STATE
         (setf (ascii85-encode-state-bits state) bits
               (ascii85-encode-state-pending state) pending
               (ascii85-encode-state-output-pending state) output-pending))
      (values (- input-index input-start) (- output-index output-start)))))

(defun octets->octets/encode/ascii85 (state output input
                                      output-start output-end
                                      input-start input-end lastp)
  (declare (type simple-octet-vector output))
  (declare (optimize speed))
  (ascii85-encoder state output input output-start output-end
                   input-start input-end lastp #'char-code))

(defun octets->string/ascii85 (state output input
                               output-start output-end
                               input-start input-end lastp)
  (declare (type simple-string output))
  (declare (optimize speed))
  (ascii85-encoder state output input output-start output-end
                   input-start input-end lastp #'identity))

(defun encode-octets-ascii85 (octets start end table writer)
  (declare (type (simple-array (unsigned-byte 8) (*)) octets))
  (declare (type index start end))
  (declare (type function writer))
  (declare (ignore table))
  (flet ((output (buffer group count)
           (if (zerop group)
               (funcall writer #\z)
               (loop for i from 4 downto 0
                 do (multiple-value-bind (g b) (truncate group 85)
                      (setf group g
                            (aref buffer i) (code-char (+ #.(char-code #\!) b))))
                  finally (dotimes (i (1+ count))
                            (funcall writer (aref buffer i)))))))
    (loop with length = (- end start)
       with buffer = (make-string 5)
       with group of-type (unsigned-byte 32) = 0
       with count of-type fixnum = 0
       with shift of-type fixnum = 24
       until (zerop length)
       do (setf group (logior group (ash (aref octets start) shift)))
         (incf start)
         (decf length)
         (decf shift 8)
         (when (= (incf count) 4)
           (output buffer group count)
           (setf group 0 count 0 shift 24))
       finally (unless (zerop count)
                 (output buffer group count)))))

(defmethod encoding-tools ((format (eql :ascii85)))
  (values #'encode-octets-ascii85 #'encoded-length-ascii85
          *ascii85-encode-table*))

(defvar *ascii85-decode-table* (make-decode-table *ascii85-encode-table*))
(declaim (type decode-table *ascii85-decode-table*))

(defun decoded-length-ascii85 (length)
  ;; FIXME: There's nothing smart we can do without scanning the string.
  ;; We have to assume the worst case, that all the characters in the
  ;; string are #\z.
  (* length 5))

(defun decode-octets-ascii85 (string start end length table writer)
  (declare (type index start end))
  (declare (type function writer))
  (declare (type decode-table table))
  (flet ((do-decode (transform)
           (do ((i 0)
                (acc 0))
               ((>= start end)
                (unless (zerop i)
                  (when (= i 1)
                    (error "corrupt ascii85 group"))
                  (dotimes (j (- 5 i))
                    (setf acc (+ (* acc 85) 84)))
                  (dotimes (j (1- i))
                    (funcall writer (ldb (byte 8 (* (- 3 j) 8)) acc)))))
             (cond
               ((>= i 5)
                (unless (< acc (ash 1 32))
                  (error "invalid ascii85 sequence"))
                (dotimes (i 4)
                  (funcall writer (ldb (byte 8 (* (- 3 i) 8)) acc)))
                (setf i 0
                      acc 0))
               (t
                (let* ((b (funcall transform (aref string start)))
                       (d (dtref table b)))
                  (incf start)
                  (cond
                    ((= b #.(char-code #\z))
                     (unless (zerop i)
                       (error "z found in the middle of an ascii85 group"))
                     (dotimes (i 4)
                       (funcall writer 0)))
                    ((= d +dt-invalid+)
                     (error "invalid ascii85 character ~X" b))
                    (t
                     (incf i)
                     (setf acc (+ (* acc 85) d))))))))))
    (declare (inline do-decode))
    (decode-dispatch string #'do-decode)))

(defmethod decoding-tools ((format (eql :ascii85)) &key case-fold map01)
  (declare (ignorable case-fold map01))
  (values #'decode-octets-ascii85 #'decoded-length-ascii85
          *ascii85-decode-table*))

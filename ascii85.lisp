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
             (:predicate nil)
             (:constructor make-ascii85-encode-state
                           (&aux (descriptor (ascii85-format-descriptor)))))
  (bits 0 :type (unsigned-byte 32))
  (pending 0 :type (integer 0 4))
  (output-group (make-array 5 :element-type 'base-char)
                :read-only t :type (simple-array base-char (5)))
  (group-index 0 :type (integer 0 4))
  (output-pending 0 :type (integer 0 5))
  (table *ascii85-encode-table* :read-only t
         :type (simple-array base-char (85))))

(defun encoded-length/ascii85 (count)
  "Return the number of characters required to encode COUNT octets in Ascii85."
  (multiple-value-bind (q r) (truncate count 4)
    (let ((complete (* q 5)))
      (if (zerop r)
          complete
          (+ complete r 1)))))

(declaim (notinline ascii85-encoder))
(defun ascii85-encoder (state output input
                        output-index output-end
                        input-index input-end lastp converter)
  (declare (type ascii85-encode-state state))
  (declare (type simple-octet-vector input))
  (declare (type index output-index output-end input-index input-end))
  (declare (type function converter))
  (let ((bits (ascii85-encode-state-bits state))
        (pending (ascii85-encode-state-pending state))
        (output-group (ascii85-encode-state-output-group state))
        (group-index (ascii85-encode-state-group-index state))
        (output-pending (ascii85-encode-state-output-pending state)))
    (declare (type index input-index output-index))
    (declare (type (unsigned-byte 32) bits))
    (declare (type (integer 0 4) pending))
    (declare (type (integer 0 5) output-pending group-index))
    (flet ((expand-for-output (bits output-group)
             (cond
               ((zerop bits)
                (setf (aref output-group 0) #\z)
                1)
               (t
                (loop for i from 4 downto 0
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
         (setf output-pending (expand-for-output bits output-group)
               group-index 0)
       OUTPUT-CHECK
         (when (>= output-index output-end)
           (go DONE))
       DO-OUTPUT
         (when (< group-index output-pending)
           (setf (aref output output-index)
                 (funcall converter
                          (aref output-group group-index)))
           (incf group-index)
           (incf output-index)
           (cond
             ((= group-index output-pending)
              (setf bits 0)
              (setf pending 0)
              (setf group-index 0)
              (setf output-pending 0)
              (go INPUT-CHECK))
             (t
              (go OUTPUT-CHECK))))
       DONE
         (unless lastp
           (go RESTORE-STATE))
         (setf (ascii85-encode-state-finished-input-p state) t)
         (setf output-pending (expand-for-output bits output-group)
               group-index 0)
       FLUSH-BITS
         (when (zerop output-pending)
           (go RESTORE-STATE))
       FLUSH-OUTPUT-CHECK
         (when (>= output-index output-end)
           (go RESTORE-STATE))
       DO-FLUSH-OUTPUT
         (when (< group-index output-pending)
           (setf (aref output output-index)
                 (funcall converter
                          (aref output-group group-index)))
           (incf group-index)
           (incf output-index)
           (cond
             ((= group-index output-pending)
              (setf bits 0)
              (setf pending 0)
              (setf group-index 0)
              (setf output-pending 0)
              (go RESTORE-STATE))
             (t
              (go FLUSH-OUTPUT-CHECK))))
       RESTORE-STATE
         (setf (ascii85-encode-state-bits state) bits
               (ascii85-encode-state-pending state) pending
               (ascii85-encode-state-group-index state) group-index
               (ascii85-encode-state-output-pending state) output-pending))
      (values input-index output-index))))

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

(defvar *ascii85-decode-table* (make-decode-table *ascii85-encode-table*))
(declaim (type decode-table *ascii85-decode-table*))

(defstruct (ascii85-decode-state
             (:include decode-state)
             (:copier nil)
             (:predicate nil)
             (:constructor %make-ascii85-decode-state
                           (&aux (descriptor (ascii85-format-descriptor)))))
  (bits 0 :type (unsigned-byte 32))
  (pending 0 :type (integer 0 5))
  (output-pending 0 :type (integer 0 4))
  (table *ascii85-decode-table* :read-only t :type decode-table))

(defun make-ascii85-decode-state (case-fold map01)
  (declare (ignore case-fold map01))
  (%make-ascii85-decode-state))

(defun ascii85-decoder (state output input
                        output-index output-end
                        input-index input-end lastp converter)
  (declare (type ascii85-decode-state state))
  (declare (type simple-octet-vector output))
  (declare (type index output-index output-end input-index input-end))
  (declare (type function converter))
  (let ((bits (ascii85-decode-state-bits state))
        (pending (ascii85-decode-state-pending state))
        (output-pending (ascii85-decode-state-output-pending state))
        (table (ascii85-decode-state-table state)))
    (declare (type (unsigned-byte 32) bits))
    (declare (type (integer 0 5) pending))
    (declare (type (integer 0 4) output-pending))
    (tagbody
     FINISHED-CHECK
       (when (ascii85-decode-state-finished-input-p state)
         (go FLUSH-BITS))
     OUTPUT-AVAILABLE-CHECK
       (when (zerop output-pending)
         (go INPUT-AVAILABLE-CHECK))
     OUTPUT-SPACE-CHECK
       (when (>= output-index output-end)
         (go DONE))
     DO-OUTPUT
       (setf (aref output output-index)
             (ldb (byte 8 (* (decf output-pending) 8)) bits))
       (incf output-index)
       (cond
         ((zerop output-pending)
          (setf bits 0)
          (setf pending 0)
          (setf output-pending 0)
          (go INPUT-AVAILABLE-CHECK))
         (t
          (go OUTPUT-SPACE-CHECK)))
     INPUT-AVAILABLE-CHECK
       (when (>= input-index input-end)
         (go DONE))
     DO-INPUT
       (cond
         ((< pending 5)
          (let* ((c (aref input input-index))
                 (v (funcall converter c))
                 (d (dtref table v)))
            (cond
              ((eql v (funcall converter #\z))
               (unless (zerop pending)
                 (error "z found in the middle of an ascii85 group"))
               (incf input-index)
               (setf output-pending 4)
               (go OUTPUT-SPACE-CHECK))
              ((= d +dt-invalid+)
               (error "invalid ascii85 character ~A at position ~D" c input-index))
              (t
               ;; FIXME: check for overflow.
               (setf bits (+ (* bits 85) d))
               (incf pending)
               (incf input-index)
               (go INPUT-AVAILABLE-CHECK)))))
         (t
          (setf output-pending 4)
          (go OUTPUT-SPACE-CHECK)))
     DONE
       (unless lastp
         (go RESTORE-STATE))
       (setf (ascii85-decode-state-finished-input-p state) t)
     EOT-VALIDITY-CHECK
       (when (zerop pending)
         (go RESTORE-STATE))
       (when (= pending 1)
         (error "invalid ascii85 input"))
       (dotimes (i (- 5 pending))
         (setf bits (+ (* bits 85) 84)))
       (setf output-pending (1- pending)
             bits (ldb (byte (* output-pending 8) (* (- 4 output-pending) 8))
                       bits))
     FLUSH-BITS
       (when (zerop output-pending)
         (go RESTORE-STATE))
     FLUSH-OUTPUT-CHECK
       (when (>= output-index output-end)
         (go RESTORE-STATE))
     DO-FLUSH-OUTPUT
       (when (> output-pending 0)
         (setf (aref output output-index)
               (ldb (byte 8 (* (decf output-pending) 8)) bits))
         (incf output-index)
         (cond
           ((zerop output-pending)
            (setf bits 0)
            (setf pending 0)
            (setf output-pending 0)
            (go RESTORE-STATE))
           (t
            (go FLUSH-OUTPUT-CHECK))))
     RESTORE-STATE
       (setf (ascii85-decode-state-bits state) bits
             (ascii85-decode-state-pending state) pending
             (ascii85-decode-state-output-pending state) output-pending))
    (values input-index output-index)))

(defun string->octets/ascii85 (state output input
                              output-index output-end
                              input-index input-end lastp)
  (declare (type simple-string input))
  (ascii85-decoder state output input output-index output-end
                   input-index input-end lastp #'char-code))

(defun octets->octets/decode/ascii85 (state output input
                                     output-index output-end
                                     input-index input-end lastp)
  (declare (type simple-octet-vector input))
  (ascii85-decoder state output input output-index output-end
                   input-index input-end lastp #'identity))

(defun decoded-length-ascii85 (length)
  ;; FIXME: There's nothing smart we can do without scanning the string.
  ;; We have to assume the worst case, that all the characters in the
  ;; string are #\z.
  (* length 5))

(register-descriptor-and-constructors :ascii85 (ascii85-format-descriptor)
                                      #'make-ascii85-encode-state
                                      #'make-ascii85-decode-state)

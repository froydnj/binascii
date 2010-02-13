;;; base64.lisp -- The base64 encoding, defined in RFC 3548 and 4648.

(cl:in-package :binascii)

(defvar *base64-encode-table*
  #.(coerce "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" 'simple-base-string))

(defvar *base64url-encode-table*
  #.(coerce "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_" 'simple-base-string))

(defun base64-format-descriptor ()
  (let* ((cell (load-time-value (list nil)))
         (fd (car cell)))
    (if fd
        fd
        (setf (car cell)
              (make-format-descriptor #'encoded-length/base64
                                      #'octets->string/base64
                                      #'octets->octets/encode/base64
                                      #'decoded-length-base64
                                      #'string->octets/base64
                                      #'octets->octets/decode/base64)))))

(defstruct (base64-encode-state
             (:include encode-state)
             (:copier nil)
             (:constructor make-base64-encode-state
                           (&aux (descriptor (base64-format-descriptor))
                                 (table *base64-encode-table*)))
             (:constructor make-base64url-encode-state
                           (&aux (descriptor (base64-format-descriptor))
                                 (table *base64url-encode-table*))))
  (bits 0 :type (unsigned-byte 16))
  (n-bits 0 :type fixnum)
  (table *base64-encode-table* :read-only t :type (simple-array base-char (64)))
  (padding-remaining 0 :type (integer 0 3)))

(declaim (inline base64-encoder))
(defun base64-encoder (state output input
                       output-start output-end
                       input-start input-end lastp converter)
  (declare (type base64-encode-state state))
  (declare (type simple-octet-vector input))
  (declare (type index output-start output-end input-start input-end))
  (declare (type function converter))
  (let* ((input-index input-start)
         (output-index output-start)
         (bits (base64-encode-state-bits state))
         (n-bits (base64-encode-state-n-bits state))
         (table (base64-encode-state-table state)))
    (declare (type index input-index output-index))
    (declare (type (unsigned-byte 16) bits))
    (declare (type fixnum n-bits))
    (tagbody
     PAD-CHECK
       (when (base64-encode-state-finished-input-p state)
         (go PAD))
     INPUT-CHECK
       (when (>= input-index input-end)
         (go DONE))
     DO-INPUT
       (when (< n-bits 6)
         (setf bits (ldb (byte 16 0)
                         (logior (ash bits 8) (aref input input-index))))
         (incf input-index)
         (incf n-bits 8))
     OUTPUT-CHECK
       (when (>= output-index output-end)
         (go DONE))
     DO-OUTPUT
       (decf n-bits 6)
       (setf (aref output output-index)
             (funcall converter (aref table (ldb (byte 6 n-bits) bits))))
       (incf output-index)
       (if (>= n-bits 6)
           (go OUTPUT-CHECK)
           (go INPUT-CHECK))
     DONE
       (unless lastp
         (go RESTORE-STATE))
       (setf (base64-encode-state-finished-input-p state) t)
       (cond
         ((= n-bits 2)
          (setf (base64-encode-state-padding-remaining state) 3))
         ((= n-bits 4)
          (setf (base64-encode-state-padding-remaining state) 2)))
     PAD
       (cond
         ((or (zerop n-bits)
              (zerop (base64-encode-state-padding-remaining state)))
          (go RESTORE-STATE))
         ((= n-bits 2)
          (go DO-PAD-FOR-2-BITS))
         ((= n-bits 4)
          (go DO-PAD-FOR-4-BITS)))
     DO-PAD-FOR-2-BITS
       (let ((padding-remaining (base64-encode-state-padding-remaining state)))
         (declare (type (integer 0 3) padding-remaining))
         (when (and (>= padding-remaining 3)
                    (< output-index output-end))
           (setf (aref output output-index)
                 (funcall converter
                          (aref table (ash (ldb (byte 2 0) bits) 4))))
           (incf output-index)
           (decf padding-remaining))
         (when (and (>= padding-remaining 2)
                    (< output-index output-end))
           (setf (aref output output-index) (funcall converter #\=))
           (incf output-index)
           (decf padding-remaining))
         (when (and (>= padding-remaining 1)
                    (< output-index output-end))
           (setf (aref output output-index) (funcall converter #\=))
           (incf output-index)
           (decf padding-remaining))
         (when (zerop padding-remaining)
           (setf n-bits 0))
         (setf (base64-encode-state-padding-remaining state) padding-remaining)
         (go RESTORE-STATE))
     DO-PAD-FOR-4-BITS
       (let ((padding-remaining (base64-encode-state-padding-remaining state)))
         (declare (type (integer 0 3) padding-remaining))
         (when (and (>= padding-remaining 2)
                    (< output-index output-end))
          (setf (aref output output-index)
                (funcall converter
                         (aref table (ash (ldb (byte 4 0) bits) 2))))
          (incf output-index)
          (decf padding-remaining))
         (when (and (>= padding-remaining 1)
                    (< output-index output-end))
           (setf (aref output output-index) (funcall converter #\=))
           (incf output-index)
           (decf padding-remaining))
         (when (zerop padding-remaining)
           (setf n-bits 0))
         (setf (base64-encode-state-padding-remaining state) padding-remaining)
         (go RESTORE-STATE))
     RESTORE-STATE
       (setf (base64-encode-state-bits state) bits
             (base64-encode-state-n-bits state) n-bits))
    (values (- input-index input-start) (- output-index output-start))))

(defun octets->octets/encode/base64 (state output input
                                     output-start output-end
                                     input-start input-end lastp)
  (declare (type simple-octet-vector output))
  (declare (optimize speed))
  (base64-encoder state output input output-start output-end
                  input-start input-end lastp #'char-code))

(defun octets->string/base64 (state output input
                              output-start output-end
                              input-start input-end lastp)
  (declare (type simple-string output))
  (declare (optimize speed))
  (base64-encoder state output input output-start output-end
                  input-start input-end lastp #'identity))

(defun encoded-length/base64 (count)
  "Return the number of characters required to encode COUNT octets in Base64."
  (* (ceiling count 3) 4))

(defvar *base64-decode-table*
  (make-decode-table *base64-encode-table*))
(declaim (type decode-table *base64-decode-table*))

(defvar *base64url-decode-table*
  (make-decode-table *base64url-encode-table*))
(declaim (type decode-table *base64url-decode-table*))

(defstruct (base64-decode-state
             (:include decode-state)
             (:copier nil)
             (:constructor %make-base64-decode-state
                           (table
                            &aux (descriptor (base64-format-descriptor)))))
  (bits 0 :type (unsigned-byte 16))
  (n-bits 0 :type (unsigned-byte 8))
  (padding-remaining 0 :type (integer 0 3))
  (table *base64-decode-table* :read-only t :type decode-table))

(defun make-base64-decode-state (case-fold map01)
  (declare (ignore case-fold map01))
  (%make-base64-decode-state *base64-decode-table*))

(defun make-base64url-decode-state (case-fold map01)
  (declare (ignore case-fold map01))
  (%make-base64-decode-state *base64url-decode-table*))

(defun base64-decoder (state output input
                       output-index output-end
                       input-index input-end lastp converter)
  (declare (type base64-decode-state state))
  (declare (type simple-octet-vector output))
  (declare (type index output-index output-end input-index input-end))
  (declare (type function converter))
  (let ((bits (base64-decode-state-bits state))
        (n-bits (base64-decode-state-n-bits state))
        (padding-remaining (base64-decode-state-padding-remaining state))
        (table (base64-decode-state-table state)))
    (declare (type (unsigned-byte 16) bits))
    (declare (type fixnum n-bits))
    (declare (type (integer 0 6) padding-remaining))
    (tagbody
     PAD-CHECK
       (when (base64-decode-state-finished-input-p state)
         (go EAT-EQUAL-CHECK-PAD))
     OUTPUT-AVAILABLE-CHECK
       (when (< n-bits 8)
         (go INPUT-AVAILABLE-CHECK))
     OUTPUT-SPACE-CHECK
       (when (>= output-index output-end)
         (go DONE))
     DO-OUTPUT
       (decf n-bits 8)
       (setf (aref output output-index) (logand (ash bits (- n-bits)) #xff)
             bits (logand bits #xff))
       (incf output-index)
       (go INPUT-AVAILABLE-CHECK)
     INPUT-AVAILABLE-CHECK
       (when (>= input-index input-end)
         (go DONE))
     DO-INPUT
       (let* ((c (aref input input-index))
              (v (funcall converter c))
              (d (dtref table v)))
         (when (= v (funcall converter #\=))
           (go SAW-EQUAL))
         (when (= d +dt-invalid+)
           (error "invalid base64 character ~A at position ~D" c input-index))
         (incf input-index)
         (setf bits (ldb (byte 16 0) (logior (ash bits 6) d)))
         (incf n-bits 6)
         (go OUTPUT-AVAILABLE-CHECK))
     DONE
       (unless lastp
         (go RESTORE-STATE))
     SAW-EQUAL
       (setf (base64-decode-state-finished-input-p state) t)
       (cond
         ((zerop n-bits)
          (go RESTORE-STATE))
         ((= n-bits 2)
          (setf padding-remaining 3))
         ((= n-bits 4)
          (setf padding-remaining 2)))
     EAT-EQUAL-CHECK-PAD
       (when (zerop padding-remaining)
         (go RESTORE-STATE))
     EAT-EQUAL-CHECK-INPUT
       (when (>= input-index input-end)
         (go RESTORE-STATE))
     EAT-EQUAL
       (let ((v (aref input input-index)))
         (unless (= (funcall converter v) (funcall converter #\=))
           (error "invalid base64 input ~A at position ~D" v input-index))
         (incf input-index)
         (decf padding-remaining)
         (go EAT-EQUAL-CHECK-PAD))
     RESTORE-STATE
       (setf (base64-decode-state-n-bits state) n-bits
             (base64-decode-state-bits state) bits
             (base64-decode-state-padding-remaining state) padding-remaining))
    (values input-index output-index)))

(defun string->octets/base64 (state output input
                              output-index output-end
                              input-index input-end lastp)
  (declare (type simple-string input))
  (base64-decoder state output input output-index output-end
                  input-index input-end lastp #'char-code))

(defun octets->octets/decode/base64 (state output input
                                     output-index output-end
                                     input-index input-end lastp)
  (declare (type simple-octet-vector input))
  (base64-decoder state output input output-index output-end
                  input-index input-end lastp #'identity))

(defun decode-octets-base64 (string start end length table writer)
  (declare (type index start end))
  (declare (type function writer))
  (declare (type decode-table table))
  (flet ((do-decode (transform)
           (loop with bits of-type (unsigned-byte 16) = 0
              with n-bits of-type (unsigned-byte 8) = 0
              for i from start below end
              for char = (aref string i)
              for value = (dtref table (funcall transform char))
              do (cond
                   ((>= value 0)
                    (setf bits (logand (logior (ash bits 6) value) #xffff))
                    (incf n-bits 6)
                    (when (>= n-bits 8)
                      (decf n-bits 8)
                      (funcall writer (logand (ash bits (- n-bits)) #xff))
                      (setf bits (logand bits #xff))))
                   ((eql (funcall transform char)
                         (funcall transform #\=)))
                   ((= value +dt-invalid+)
                    (error "bad character ~A in base64 decoding" char))))))
    (declare (inline do-decode))
    (decode-dispatch string #'do-decode)))

(defun decoded-length-base64 (length)
  (* (ceiling length 4) 3))

(defmethod decoding-tools ((format (eql :base64)) &key case-fold map01)
  (declare (ignorable case-fold map01))
  (values #'decode-octets-base64 #'decoded-length-base64
          *base64-decode-table*))

(defmethod decoding-tools ((format (eql :base64url)) &key case-fold map01)
  (declare (ignorable case-fold map01))
  (values #'decode-octets-base64 #'decoded-length-base64
          *base64url-decode-table*))

(register-descriptor-and-constructors :base64 (base64-format-descriptor)
                                      #'make-base64-encode-state
                                      #'make-base64-decode-state)
(register-descriptor-and-constructors :base64url (base64-format-descriptor)
                                      #'make-base64url-encode-state
                                      #'make-base64url-decode-state)

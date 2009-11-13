;;; base64.lisp -- The base64 encoding, defined in RFC 3548 and 4648.

(cl:in-package :binascii)

(defvar *base64-encode-table*
  #.(coerce "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" 'simple-base-string))

(defvar *base64url-encode-table*
  #.(coerce "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_" 'simple-base-string))

(defun encode-octets-base64 (octets start end table writer)
  (declare (type (simple-array (unsigned-byte 8) (*)) octets))
  (declare (type index start end))
  (declare (type function writer))
  (declare (type (simple-array base-char (64)) table))
  (loop 
     for i from start below end
     for bits of-type (unsigned-byte 16) = (aref octets i)
       then (ldb (byte 16 0) (logior (ash bits 8) (aref octets i)))
     for n-bits of-type fixnum = 8 then (+ n-bits 8)
     do (loop while (>= n-bits 6)
           do (decf n-bits 6)
           (funcall writer (aref table (ldb (byte 6 n-bits) bits))))
     finally (cond
               ((= n-bits 2)
                (funcall writer (aref table (ash (ldb (byte 2 0) bits) 4)))
                (funcall writer #\=)
                (funcall writer #\=))
               ((= n-bits 4)
                (funcall writer (aref table (ash (ldb (byte 4 0) bits) 2)))
                (funcall writer #\=)))))

(defun encoded-length-base64 (count)
  "Return the number of characters required to encode COUNT octets in Base64."
  (* (ceiling count 3) 4))

(defmethod encoding-tools ((format (eql :base64)))
  (values #'encode-octets-base64 #'encoded-length-base64
          *base64-encode-table*))

(defmethod encoding-tools ((format (eql :base64url)))
  (values #'encode-octets-base64 #'encoded-length-base64
          *base64url-encode-table*))

(defvar *base64-decode-table*
  (make-decode-table *base64-encode-table*))
(declaim (type decode-table *base64-decode-table*))

(defvar *base64url-decode-table*
  (make-decode-table *base64url-encode-table*))
(declaim (type decode-table *base64url-decode-table*))

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

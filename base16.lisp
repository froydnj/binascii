;;;; base16.lisp -- The base16 encoding, formalized in RFC 3548 and RFC 4648.

(cl:in-package :binascii)

(defvar *hex-encode-table*
  #.(coerce "0123456789abcdef" 'simple-base-string))
(defvar *base16-encode-table*
  #.(coerce "0123456789ABCDEF" 'simple-base-string))

(defvar *base16-decode-table*
  (make-decode-table *base16-encode-table*))
(declaim (type decode-table *base16-decode-table*))

(defun encoded-length-base16 (count)
  "Return the number of characters required to encode COUNT octets in Base16."
  (* count 2))

(defun encode-octets-base16 (octets start end table writer)
  (declare (type (simple-array (unsigned-byte 8) (*)) octets))
  (declare (type index start end))
  (declare (type function writer))
  (declare (type (simple-array base-char (16)) table))
  (loop for i from start below end
     do (let ((byte (aref octets i)))
          (funcall writer (aref table (ldb (byte 4 4) byte)))
          (funcall writer (aref table (ldb (byte 4 0) byte))))))

(defmethod encoding-tools ((format (eql :base16)))
  (values #'encode-octets-base16 #'encoded-length-base16
          *base16-encode-table*))

(defmethod encoding-tools ((format (eql :hex)))
  (values #'encode-octets-base16 #'encoded-length-base16
          *hex-encode-table*))

(defun decode-octets-base16 (string start end length table writer)
  (declare (type index start end))
  (declare (type function writer))
  (declare (type decode-table table))
  (declare (optimize (speed 3)))
  (flet ((do-decode (transform)
           (loop for i from start below end by 2
              do (let* ((char1 (aref string i))
                        (char2 (aref string (1+ i)))
                        (v1 (dtref table (funcall transform char1)))
                        (v2 (dtref table (funcall transform char2))))
                   (when (= v1 +dt-invalid+)
                     (error "Invalid hex digit ~A" char1))
                   (when (= v2 +dt-invalid+)
                     (error "Invalid hex digit ~A" char2))
                   (funcall writer (+ (* v1 16) v2))))))
    (declare (inline do-decode))
    (decode-dispatch string #'do-decode)))

(defun decoded-length-base16 (length)
  (unless (evenp length)
    (error "cannot decode an odd number of base16 characters"))
  (truncate length 2))

(defmethod decoding-tools ((format (eql :base16)) &key case-fold map01)
  (declare (ignorable case-fold map01))
  (values #'decode-octets-base16
          #'decoded-length-base16
          (if case-fold
              (case-fold-decode-table *base16-decode-table*
                                      *base16-encode-table*)
              *base16-decode-table*)))

(defmethod decoding-tools ((format (eql :hex)) &key case-fold map01)
  (declare (ignorable case-fold map01))
  (values #'decode-octets-base16
          #'decoded-length-base16
          (case-fold-decode-table *base16-decode-table*
                                  *base16-encode-table*)))

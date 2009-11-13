;;; base85.lisp -- base85 encoding, in the flavor that git uses

(cl:in-package :binascii)

(defvar *base85-encode-table*
  #.(coerce "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!#$%&()*+-;<=>?@^_`{|}~" 'simple-base-string))

(defun encoded-length-base85 (count)
  "Return the number of characters required to encode COUNT octets in Base85."
  (* (ceiling count 4) 5))

(defun encode-octets-base85 (octets start end table writer)
  (declare (type (simple-array (unsigned-byte 8) (*)) octets))
  (declare (type index start end))
  (declare (type (simple-array base-char (85)) table))
  (declare (type function writer))
  (loop with length = (- end start)
     with buffer = (make-string 5)
     while (plusp length)
     do (let ((group (do ((g 0)
                          (i 24 (- i 8)))
                         ((or (zerop length) (< i 0)) g)
                       (setf g (logior (ash (aref octets start) i) g))
                       (incf start)
                       (decf length))))
          (loop for i from 4 downto 0
             do (multiple-value-bind (g b) (truncate group 85)
                  (setf group g
                        (aref buffer i) (aref table b)))
             finally (dotimes (i 5)
                       (funcall writer (aref buffer i)))))))

(defmethod encoding-tools ((format (eql :base85)))
  (values #'encode-octets-base85 #'encoded-length-base85
          *base85-encode-table*))

(defvar *base85-decode-table* (make-decode-table *base85-encode-table*))
(declaim (type decode-table *base85-decode-table*))

(defun decoded-length-base85 (length)
  (multiple-value-bind (n-groups rem) (truncate length 5)
    (unless (zerop rem)
      (error "base85 input length ~D must be a multiple of 5" length))
    (* n-groups 4)))

(defun decode-octets-base85 (string start end length table writer)
  (declare (type index start end))
  (declare (type function writer))
  (declare (type decode-table table))
  (flet ((do-decode (transform)
           (loop while (< start end)
              do (do ((i 0 (1+ i))
                      (acc 0))
                     ((>= i 5)
                      (incf start 5)
                      (unless (< acc (ash 1 32))
                        (error "invalid base85 sequence"))
                      (let ((count (min length 4))) 
                        (dotimes (i count)
                          (funcall writer (ldb (byte 8 (* (- 3 i) 8)) acc)))
                        (decf length count)))
                   (let* ((b (funcall transform (aref string (+ start i))))
                          (d (dtref table b)))
                     (when (= d +dt-invalid+)
                       (error "invalid base85 character: ~X" b))
                     (setf acc (+ (* acc 85) d)))))))
    (declare (inline do-decode))
    (decode-dispatch string #'do-decode)))

(defmethod decoding-tools ((format (eql :base85)) &key case-fold map01)
  (declare (ignorable case-fold map01))
  (values #'decode-octets-base85 #'decoded-length-base85
          *base85-decode-table*))

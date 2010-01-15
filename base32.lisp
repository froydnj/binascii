;;;; base32.lisp -- The base32 encoding, defined in RFC 3548 and 4648.

(cl:in-package :binascii)

(defvar *base32-encode-table*
  #.(coerce "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567" 'simple-base-string))
(defvar *base32hex-encode-table*
  #.(coerce "0123456789ABCDEFGHIJKLMNOPQRSTUV" 'simple-base-string))

(defstruct (base32-encode-state
             (:include encode-state)
             (:copier nil)
             (:constructor make-base32-encode-state
                           (table
                            &aux (encoded-length #'encoded-length/base32)
                            (octets->octets #'octets->octets/base32)
                            (octets->string #'octets->string/base32))))
  (bits 0 :type (unsigned-byte 16))
  (n-bits 0 :type fixnum)
  (table *base32-encode-table* :read-only t
         :type (simple-array base-char (32)))
  (adding-padding-p nil)
  (padding-remaining 0 :type (integer 0 6)))

(declaim (inline base32-encoder))
(defun base32-encoder (state output input
                       output-start output-end
                       input-start input-end lastp converter)
  (declare (type base32-encode-state state))
  (declare (type simple-octet-vector input))
  (declare (type index output-start output-end input-start input-end))
  (declare (type function converter))
  (let* ((input-index input-start)
         (output-index output-start)
         (bits (base32-encode-state-bits state))
         (n-bits (base32-encode-state-n-bits state))
         (table (base32-encode-state-table state))
         (n-pad-chars #.(make-array 5 :initial-contents '(0 4 1 6 3)
                                    :element-type 'fixnum)))
                      
    (declare (type index input-index output-index))
    (declare (type (unsigned-byte 16) bits))
    (declare (type fixnum n-bits))
    (declare (type (simple-array fixnum (5)) n-pad-chars))
    (tagbody
       PAD-CHECK
       (when (base32-encode-state-adding-padding-p state)
         (go PAD))
       INPUT-CHECK
       (when (>= input-index input-end)
         (go DONE))
     DO-INPUT
       (when (< n-bits 5)
         (setf bits (ldb (byte 16 0)
                         (logior (ash bits 8) (aref input input-index))))
         (incf input-index)
         (incf n-bits 8))
     OUTPUT-CHECK
       (when (>= output-index output-end)
         (go DONE))
     DO-OUTPUT
       (decf n-bits 5)
       (setf (aref output output-index)
             (funcall converter (aref table (ldb (byte 5 n-bits) bits))))
       (incf output-index)
       (if (>= n-bits 5)
           (go OUTPUT-CHECK)
           (go INPUT-CHECK))
     DONE
       (unless lastp
         (go RESTORE-STATE))
       (setf (base32-encode-state-adding-padding-p state) t)
       (setf (base32-encode-state-padding-remaining state)
             (aref n-pad-chars n-bits))
     PAD
       (locally (declare (type (integer 0 4) n-bits))
         (let ((padding-remaining (base32-encode-state-padding-remaining state))
               (max-pad-chars (aref n-pad-chars n-bits)))
           (declare (type (integer 0 6) padding-remaining))
           (declare (type (integer 0 6) max-pad-chars))
           (when (and (= padding-remaining max-pad-chars)
                      (< output-index output-end))
             (setf (aref output output-index)
                   (funcall converter
                            (aref table (ash (ldb (byte n-bits 0) bits)
                                             (- 5 n-bits)))))
             (incf output-index)
             (decf padding-remaining))
           (when (< padding-remaining max-pad-chars)
             (loop while (and (>= padding-remaining 0)
                              (< output-index output-end))
                do (setf (aref output output-index) (funcall converter #\=))
                  (incf output-index)
                  (decf padding-remaining)))
           (when (zerop padding-remaining)
             (setf n-bits 0))
           (setf (base32-encode-state-padding-remaining state) padding-remaining)))
     RESTORE-STATE
       (setf (base32-encode-state-bits state) bits
             (base32-encode-state-n-bits state) n-bits))
    (values (- input-index input-start) (- output-index output-start))))

(defun octets->octets/base32 (state output input
                              output-start output-end
                              input-start input-end lastp)
  (declare (type simple-octet-vector output))
  (declare (optimize speed))
  (base32-encoder state output input output-start output-end
                  input-start input-end lastp #'char-code))

(defun octets->string/base32 (state output input
                              output-start output-end
                              input-start input-end lastp)
  (declare (type simple-string output))
  (declare (optimize speed))
  (base32-encoder state output input output-start output-end
                  input-start input-end lastp #'identity))

(defun encode-octets-base32 (octets start end table writer)
  (declare (type (simple-array (unsigned-byte 8) (*)) octets))
  (declare (type index start end))
  (declare (type function writer))
  (declare (type (simple-array base-char (32)) table))
  (loop for i from start below end
     for bits of-type (unsigned-byte 16) = (aref octets i)
     then (ldb (byte 16 0) (logior (ash bits 8) (aref octets i)))
     for n-bits of-type fixnum = 8 then (+ n-bits 8)
     do (loop while (>= n-bits 5)
           do (decf n-bits 5)
           (funcall writer (aref table (ldb (byte 5 n-bits) bits))))
     finally (let ((n-pad
                    (case n-bits
                      (3
                       (funcall writer (aref table
                                             (ash (ldb (byte 3 0) bits) 2)))
                       6)
                      (1
                       (funcall writer (aref table
                                             (ash (ldb (byte 1 0) bits) 4)))
                       4)
                      (4
                       (funcall writer (aref table
                                             (ash (ldb (byte 4 0) bits) 1)))
                       3)
                      (2
                       (funcall writer (aref table
                                             (ash (ldb (byte 2 0) bits) 3)))
                       1)
                      (otherwise 0))))
               (dotimes (i n-pad)
                 (funcall writer #\=)))))

(defun encoded-length/base32 (count)
  "Return the number of characters required to encode COUNT octets in Base32."
  (* (ceiling count 5) 8))

(defmethod encoding-tools ((format (eql :base32)))
  (values #'encode-octets-base32 #'encoded-length/base32
          *base32-encode-table*))

(defmethod encoding-tools ((format (eql :base32hex)))
  (values #'encode-octets-base32 #'encoded-length/base32
          *base32hex-encode-table*))

(defvar *base32-decode-table* (make-decode-table *base32-encode-table*))
(defvar *base32hex-decode-table* (make-decode-table *base32hex-encode-table*))
(declaim (type decode-table *base32-decode-table* *base32hex-decode-table*))

(defun decode-octets-base32 (string start end length table writer)
  (declare (type index start end))
  (declare (type decode-table table))
  (declare (type function writer))
  (flet ((do-decode (transform)
           (loop with bits of-type (unsigned-byte 16) = 0
              with n-bits of-type (unsigned-byte 8) = 0
              for i from start below end
              for char = (aref string i)
              for value = (dtref table (funcall transform char))
              do (cond
                   ((>= value 0)
                    (setf bits (logand (logior (ash bits 5) value) #xffff))
                    (incf n-bits 5)
                    (when (>= n-bits 8)
                      (decf n-bits 8)
                      (funcall writer (logand (ash bits (- n-bits)) #xff))
                      (setf bits (logand bits #xff))))
                   ((eql (funcall transform char)
                         (funcall transform #\=)))
                   (t
                    (error "bad character ~A in base32 decoding" char))))))
    (declare (inline do-decode))
    (decode-dispatch string #'do-decode)))

(defun decoded-length-base32 (length)
  (* (ceiling length 8) 5))

(defmethod decoding-tools ((format (eql :base32)) &key case-fold map01)
  (declare (ignorable case-fold map01))
  (values #'decode-octets-base32
          #'decoded-length-base32
          (let ((table *base32-decode-table*))
            (when map01
              (setf table (copy-seq table))
              (setf (aref table (char-code #\0)) (aref table (char-code #\O)))
              (case map01
                ((#\I #\L) (setf (aref table (char-code #\1))
                                 (aref table (char-code map01))))))
            (when case-fold
              (setf table (case-fold-decode-table table *base32-encode-table*)))
            table)))

(defmethod decoding-tools ((format (eql :base32hex)) &key case-fold map01)
  (declare (ignorable case-fold map01))
  (values #'decode-octets-base32 #'decoded-length-base32
          *base32hex-decode-table*))

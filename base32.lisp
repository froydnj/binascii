;;;; base32.lisp -- The base32 encoding, defined in RFC 3548 and 4648.

(cl:in-package :binascii)

(defvar *base32-encode-table*
  #.(coerce "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567" 'simple-base-string))
(defvar *base32hex-encode-table*
  #.(coerce "0123456789ABCDEFGHIJKLMNOPQRSTUV" 'simple-base-string))

(defun base32-format-descriptor ()
  (let* ((cell (load-time-value (list nil)))
         (fd (car cell)))
    (if fd
        fd
        (setf (car cell)
              (make-format-descriptor #'encoded-length/base32
                                      #'octets->string/base32
                                      #'octets->octets/encode/base32
                                      #'decoded-length-base32
                                      #'string->octets/base32
                                      #'octets->octets/decode/base32)))))

(defstruct (base32-encode-state
             (:include encode-state)
             (:copier nil)
             (:predicate nil)
             (:constructor make-base32-encode-state
                           (&aux (descriptor (base32-format-descriptor))
                                 (table *base32-encode-table*)))
             (:constructor make-base32hex-encode-state
                           (&aux (descriptor (base32-format-descriptor))
                                 (table *base32hex-encode-table*))))
  (bits 0 :type (unsigned-byte 16))
  (n-bits 0 :type (unsigned-byte 8))
  (table *base32-encode-table* :read-only t
         :type (simple-array base-char (32)))
  (padding-remaining 0 :type (integer 0 6)))

(declaim (inline base32-encoder))
(defun base32-encoder (state output input
                       output-index output-end
                       input-index input-end lastp converter)
  (declare (type base32-encode-state state))
  (declare (type simple-octet-vector input))
  (declare (type index output-index output-end input-index input-end))
  (declare (type function converter))
  (let ((bits (base32-encode-state-bits state))
        (n-bits (base32-encode-state-n-bits state))
        (table (base32-encode-state-table state))
        (n-pad-chars #.(make-array 5 :initial-contents '(0 4 1 6 3)
                                   :element-type 'fixnum)))
                      
    (declare (type index input-index output-index))
    (declare (type (unsigned-byte 16) bits))
    (declare (type (unsigned-byte 8) n-bits))
    (declare (type (simple-array fixnum (5)) n-pad-chars))
    (tagbody
     PAD-CHECK
       (when (base32-encode-state-finished-input-p state)
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
       (setf (base32-encode-state-finished-input-p state) t)
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
             (incf output-index))
           (loop while (and (> padding-remaining 0)
                            (< output-index output-end))
              do (setf (aref output output-index) (funcall converter #\=))
              (incf output-index)
              (decf padding-remaining))
           (when (zerop padding-remaining)
             (setf n-bits 0))
           (setf (base32-encode-state-padding-remaining state) padding-remaining)))
     RESTORE-STATE
       (setf (base32-encode-state-bits state) bits
             (base32-encode-state-n-bits state) n-bits))
    (values input-index output-index)))

(defun octets->octets/encode/base32 (state output input
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

(defvar *base32-decode-table* (make-decode-table *base32-encode-table*))
(defvar *base32hex-decode-table* (make-decode-table *base32hex-encode-table*))
(declaim (type decode-table *base32-decode-table* *base32hex-decode-table*))

(defun base32-decode-table (case-fold map01)
  (let ((table *base32-decode-table*))
    (when map01
      (setf table (copy-seq table))
      (setf (aref table (char-code #\0)) (aref table (char-code #\O)))
      (case map01
        ((#\I #\L) (setf (aref table (char-code #\1))
                         (aref table (char-code map01))))))
    (when case-fold
      (setf table (case-fold-decode-table table *base32-encode-table*)))
    table))

(defstruct (base32-decode-state
             (:include decode-state)
             (:copier nil)
             (:predicate nil)
             (:constructor %make-base32-decode-state
                           (table
                            &aux (descriptor (base32-format-descriptor)))))
  (bits 0 :type (unsigned-byte 16))
  (n-bits 0 :type (unsigned-byte 8))
  (padding-remaining 0 :type (integer 0 6))
  (table *base32-decode-table* :read-only t :type decode-table))

(defun make-base32-decode-state (case-fold map01)
  (%make-base32-decode-state (base32-decode-table case-fold map01)))

(defun make-base32hex-decode-state (case-fold map01)
  (declare (ignore case-fold map01))
  (%make-base32-decode-state *base32hex-decode-table*))

(defun base32-decoder (state output input
                       output-index output-end
                       input-index input-end lastp converter)
  (declare (type base32-decode-state state))
  (declare (type simple-octet-vector output))
  (declare (type index output-index output-end input-index input-end))
  (declare (type function converter))
  (let ((bits (base32-decode-state-bits state))
        (n-bits (base32-decode-state-n-bits state))
        (padding-remaining (base32-decode-state-padding-remaining state))
        (table (base32-decode-state-table state)))
    (declare (type (unsigned-byte 16) bits))
    (declare (type fixnum n-bits))
    (declare (type (integer 0 6) padding-remaining))
    (tagbody
     PAD-CHECK
       (when (base32-decode-state-finished-input-p state)
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
         (when (= v (if (typep input 'simple-octet-vector)
                        (char-code #\=)
                        (funcall converter #\=)))
           (go SAW-EQUAL))
         (when (= d +dt-invalid+)
           (error "invalid base32 character ~A at position ~D" c input-index))
         (incf input-index)
         (setf bits (ldb (byte 16 0) (logior (ash bits 5) d)))
         (incf n-bits 5)
         (go OUTPUT-AVAILABLE-CHECK))
     DONE
       (unless lastp
         (go RESTORE-STATE))
     SAW-EQUAL
       (setf (base32-decode-state-finished-input-p state) t)
       ;; A complete base32 group is:
       ;;
       ;; vvvvvvvv wwwwwwww xxxxxxxx yyyyyyyy zzzzzzzz
       ;;
       ;; which gets encoded by:
       ;;
       ;; vvvvv vvvww wwwww wxxxx xxxxy yyyyy yyzzz zzzzz
       ;;
       ;; so the intermediate bits left are: 3 1 4 2 0
       ;; corresponding to padding amounts : 6 4 3 1 0 (in characters)
       ;;
       ;; but we also have to handle cases where we start padding too
       ;; soon: we can't handle padding after seeing 1 group of 5, 3
       ;; groups of 5, 4 groups of 5 or 6 groups of five.  those
       ;; correspond to 5 bits remaining (having not seen the 3 v's), 7
       ;; bits remaining (having not seen the 1 w), 4 bits remaining
       ;; (having not seen the 4 x's), and 6 bits remaining (having not
       ;; seen the 2 y's).
       (let ((n-pad-chars #.(make-array 5 :initial-contents '(0 4 1 6 3)
                                        :element-type 'fixnum)))
         (if (<= n-bits 4)
             (setf padding-remaining (aref n-pad-chars n-bits))
             (error "invalid base32 input")))
     EAT-EQUAL-CHECK-PAD
       (when (zerop padding-remaining)
         (go RESTORE-STATE))
     EAT-EQUAL-CHECK-INPUT
       (when (>= input-index input-end)
         (go RESTORE-STATE))
     EAT-EQUAL
       (let ((v (aref input input-index)))
         (unless (= (funcall converter v)
                    (if (typep input 'simple-octet-vector)
                        (char-code #\=)
                        (funcall converter #\=)))
           (error "invalid base32 input ~A at position ~D" v input-index))
         (incf input-index)
         (decf padding-remaining)
         (go EAT-EQUAL-CHECK-PAD))
     RESTORE-STATE
       (setf (base32-decode-state-n-bits state) n-bits
             (base32-decode-state-bits state) bits
             (base32-decode-state-padding-remaining state) padding-remaining))
    (values input-index output-index)))

(defun string->octets/base32 (state output input
                              output-index output-end
                              input-index input-end lastp)
  (declare (type simple-string input))
  (base32-decoder state output input output-index output-end
                  input-index input-end lastp #'char-code))

(defun octets->octets/decode/base32 (state output input
                                     output-index output-end
                                     input-index input-end lastp)
  (declare (type simple-octet-vector input))
  (base32-decoder state output input output-index output-end
                  input-index input-end lastp #'identity))

(defun encoded-length/base32 (count)
  "Return the number of characters required to encode COUNT octets in Base32."
  (* (ceiling count 5) 8))

(defun decoded-length-base32 (length)
  (* (ceiling length 8) 5))

(define-format :base32 base32-format-descriptor
  make-base32-encode-state make-base32-decode-state)
(define-format :base32hex base32-format-descriptor
  make-base32hex-encode-state make-base32hex-decode-state)

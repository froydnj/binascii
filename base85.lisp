;;; base85.lisp -- base85 encoding, in the flavor that git uses

(cl:in-package :binascii)

(defvar *base85-encode-table*
  #.(coerce "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!#$%&()*+-;<=>?@^_`{|}~" 'simple-base-string))

(defun base85-format-descriptor ()
  (let ((cell (load-time-value (list nil)))
        (fd (car cell)))
    (if fd
        fd
        (setf (car cell)
              (make-format-descriptor #'encoded-length/base85
                                      #'octets->string/base85
                                      #'octets->octets/encode/base85
                                      #'decoded-length-base85
                                      #'string->octets/base85
                                      #'octets->octets/decode/base85)))))

(defstruct (base85-encode-state
             (:include encode-state)
             (:copier nil)
             (:constructor make-base85-encode-state
                           (&aux (descriptor (base85-format-descriptor)))))
  ;; TODO: Clever hack for little-endian machines: fill in GROUP
  ;; back-to-front, using PENDING to count down, then use SBCL's
  ;; %VECTOR-RAW-BITS or similar to read out the group in proper
  ;; big-endian order.  We could even do the same thing on x86-64 if we
  ;; made the buffer bigger.
  ;;
  ;; For now, though, we'll fill GROUP front-to-back and PENDING will
  ;; indicate how many octets we've filled in.
  #+nil
  (group (make-array 4 :element-type '(unsigned-byte 8))
         :read-only t :type (simple-array (unsigned-byte 8) (4)))
  (bits 0 :type (unsigned-byte 32))
  (pending 0 :type (integer 0 4))
  (output-group (make-array 5 :element-type 'base-char)
                :read-only t :type (simple-array base-char (5)))
  (output-pending 0 :type (integer 0 5))
  (table *base85-encode-table* :read-only t :type (simple-array base-char (85))))

(defun encoded-length/base85 (count)
  "Return the number of characters required to encode COUNT octets in Base85."
  (* (ceiling count 4) 5))

(declaim (inline base85-encode))
(defun base85-encoder (state output input
                       output-start output-end
                       input-start input-end lastp converter)
  (declare (type base85-encode-state state))
  (declare (type simple-octet-vector input))
  (declare (type index output-start output-end input-start input-end))
  (declare (type function converter))
  (let ((input-index input-start)
        (output-index output-start)
        (bits (base85-encode-state-bits state))
        (pending (base85-encode-state-pending state))
        (output-group (base85-encode-state-output-group state))
        (output-pending (base85-encode-state-output-pending state))
        (table (base85-encode-state-table state)))
    (declare (type index input-index output-index))
    (declare (type (unsigned-byte 32) bits))
    (declare (type (integer 0 4) pending))
    (declare (type (integer 0 5) output-pending))
    (flet ((expand-for-output (bits output-group)
             (loop for i from 4 downto 0
                do (multiple-value-bind (b index) (truncate bits 85)
                     (setf bits b
                           (aref output-group i) (aref table index)))
                finally (setf output-pending 5))))
      (declare (inline expand-for-output))
      (tagbody
       PAD-CHECK
         (when (base85-encode-state-finished-input-p state)
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
         (expand-for-output bits output-group)
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
         (setf (base85-encode-state-finished-input-p state) t)
         ;; Make it appear as though the input were padded with zeros to a
         ;; full input group.
         (let ((for-pad (- 4 pending)))
           (setf bits (ldb (byte 32 0) (ash bits (* 8 for-pad))))
           (setf pending 4)
           (expand-for-output bits output-group))
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
         (setf (base85-encode-state-bits state) bits
               (base85-encode-state-pending state) pending
               (base85-encode-state-output-pending state) output-pending))
      (values (- input-index input-start) (- output-index output-start)))))

(defun octets->octets/encode/base85 (state output input
                                     output-start output-end
                                     input-start input-end lastp)
  (declare (type simple-octet-vector output))
  (declare (optimize speed))
  (base85-encoder state output input output-start output-end
                  input-start input-end lastp #'char-code))

(defun octets->string/base85 (state output input
                              output-start output-end
                              input-start input-end lastp)
  (declare (type simple-string output))
  (declare (optimize speed))
  (base85-encoder state output input output-start output-end
                  input-start input-end lastp #'identity))

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
  (values #'encode-octets-base85 #'encoded-length/base85
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

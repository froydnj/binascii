;;;; streams.lisp -- a gray streams interface to octet encoding.

(cl:in-package :binascii)


;;; portability definitions

#+cmu
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :gray-streams))

;;; TRIVIAL-GRAY-STREAMS has it, we might as well, too...
#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'stream:stream-write-string)
    (require "streamc.fasl")))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defvar *binary-input-stream-class*
  (quote
   #+lispworks stream:fundamental-binary-input-stream
   #+sbcl sb-gray:fundamental-binary-input-stream
   #+openmcl gray:fundamental-binary-input-stream
   #+cmu ext:fundamental-binary-input-stream
   #+allegro excl:fundamental-binary-input-stream
   #-(or lispworks sbcl openmcl cmu allegro)
   (error "octet streams not supported in this implementation")))

(defvar *binary-output-stream-class*
  (quote
   #+lispworks stream:fundamental-binary-output-stream
   #+sbcl sb-gray:fundamental-binary-output-stream
   #+openmcl gray:fundamental-binary-output-stream
   #+cmu ext:fundamental-binary-output-stream
   #+allegro excl:fundamental-binary-output-stream
   #-(or lispworks sbcl openmcl cmu allegro)
   (error "octet streams not supported in this implementation")))

;;; FIXME: how to do CMUCL support for this?
(defvar *stream-element-type-function*
  (quote
   #+lispworks cl:stream-element-type
   #+sbcl sb-gray::stream-element-type
   #+openmcl cl:stream-element-type
   #+cmu cl:stream-element-type
   #+allegro cl:stream-element-type
   #-(or lispworks sbcl openmcl cmu allegro)
   (error "octet streams not supported in this implementation")))

(defvar *stream-read-byte-function*
  (quote
   #+lispworks stream:stream-read-byte
   #+sbcl sb-gray:stream-read-byte
   #+openmcl gray:stream-read-byte
   #+cmu ext:stream-read-byte
   #+allegro excl:stream-read-byte
   #-(or lispworks sbcl openmcl cmu allegro)
   (error "octet streams not supported in this implementation")))

(defvar *stream-write-byte-function*
  (quote
   #+lispworks stream:stream-write-byte
   #+sbcl sb-gray:stream-write-byte
   #+openmcl gray:stream-write-byte
   #+cmu ext:stream-write-byte
   #+allegro excl:stream-write-byte
   #-(or lispworks sbcl openmcl cmu allegro)
   (error "octet streams not supported in this implementation")))

(defvar *stream-read-sequence-function*
  (quote
   #+lispworks stream:stream-read-sequence
   #+sbcl sb-gray:stream-read-sequence
   #+openmcl ccl:stream-read-vector
   #+cmu ext:stream-read-sequence
   #+allegro excl:stream-read-sequence
   #-(or lispworks sbcl openmcl cmu allegro)
   (error "octet streams not supported in this implementation")))

(defvar *stream-write-sequence-function*
  (quote
   #+lispworks stream:stream-write-sequence
   #+sbcl sb-gray:stream-write-sequence
   #+openmcl ccl:stream-write-vector
   #+cmu ext:stream-write-sequence
   #+allegro excl:stream-write-sequence
   #-(or lispworks sbcl openmcl cmu allegro)
   (error "octet streams not supported in this implementation")))

(defvar *stream-finish-output-function*
  (quote
   #+lispworks stream:stream-finish-output
   #+sbcl sb-gray:stream-finish-output
   #+openmcl gray:stream-finish-output
   #+cmu ext:stream-finish-output
   #+allegro excl:stream-finish-output
   #-(or lispworks sbcl openmcl cmu allegro)
   (error "octet streams not supported in this implementation")))

(defvar *stream-force-output-function*
  (quote
   #+lispworks stream:stream-force-output
   #+sbcl sb-gray:stream-force-output
   #+openmcl gray:stream-force-output
   #+cmu ext:stream-force-output
   #+allegro excl:stream-force-output
   #-(or lispworks sbcl openmcl cmu allegro)
   (error "octet streams not supported in this implementation")))

(defvar *stream-clear-output-function*
  (quote
   #+lispworks stream:stream-clear-output
   #+sbcl sb-gray:stream-clear-output
   #+openmcl gray:stream-clear-output
   #+cmu ext:stream-clear-output
   #+allegro excl:stream-clear-output
   #-(or lispworks sbcl openmcl cmu allegro)
   (error "octet streams not supported in this implementation")))
)

(defmacro define-stream-write-sequence (specializer type &body body)
  #+sbcl
  `(defmethod sb-gray:stream-write-sequence ((stream ,specializer) seq &optional (start 0) end)
     (typecase seq
       (,type
        (let ((end (or end (length seq))))
          ,@body))
       (t
        (call-next-method))))
  #+cmu
  `(defmethod ext:stream-write-sequence ((stream ,specializer) seq &optional (start 0) end)
     (typecase seq
       (,type
        (let ((end (or end (length seq))))
          ,@body))
       (t
        (call-next-method))))
  #+allegro
  `(defmethod stream:stream-write-sequence ((stream ,specializer) seq &optional (start 0) end)
     (typecase seq
       (,type
        (let ((end (or end (length seq))))
          ,@body))
       (t
        (call-next-method))))
  #+openmcl
  `(defmethod ccl:stream-write-vector ((stream ,specializer) seq start end)
     (typecase seq
       (,type
        ,@body)
       (t
        (call-next-method))))
  #+lispworks
  `(defmethod stream:stream-write-sequence ((stream ,specializer) seq start end)
     (typecase seq
       (,type
        ,@body)
       (t
        (call-next-method)))))

;;; encoding streams

(defclass encoding-stream (#.*binary-output-stream-class*)
  ((buffer :reader buffer :initarg :buffer)
   (index :accessor index :initform 0)
   (encode-fun :reader encode-fun :initarg :encode-fun)
   (encode-table :reader encode-table :initarg :encode-table)
   (writer :reader writer :initarg :writer)))

(defun make-encoding-stream (destination format &key (element-type 'base-char))
  (multiple-value-bind (encode-fun length-fun table) (encoding-tools format)
    (let ((canonical-element-type (canonicalize-element-type element-type)))
      ;; FIXME: is it worth supporting this case and saving the user a
      ;; smidgen of coding?
      (when (null destination)
        (case canonical-element-type
          ((character base-char)
           (setf destination (make-string-output-stream
                              :element-type canonical-element-type)))
          (octet
           ;; No octet streams in CLHS, so just output to an adjustable
           ;; octet vector.
           (setf destination (make-array 128 :element-type '(unsigned-byte 8)
                                         :fill-pointer 0 :adjustable t)))))
      (multiple-value-bind (writer return-value)
          (determine-encoding-writer destination 0 canonical-element-type)
        (declare (ignore return-value))
        (make-instance 'encoding-stream
                       :encode-fun encode-fun
                       :encode-table table
                       :writer writer)))))

(defmethod #.*stream-write-byte-function* ((stream encoding-stream) byte)
  (let ((v (make-array 1 :element-type '(unsigned-byte 8)
                       :initial-element byte)))
    (write-sequence v stream)))

(define-stream-write-sequence encoding-stream (simple-array (unsigned-byte 8) (*))
  (loop with buffer = (buffer stream)
     with index = (index stream)
     with chunk-size = (length buffer)
     with length = (- end start)
     with encode-fun of-type function = (encode-fun stream)
     with table = (encode-table stream)
     with writer = (writer stream)
     initially (unless (zerop index)
                 (let ((to-copy (min (- chunk-size index) length)))
                   (replace buffer seq :start1 index :start2 start :end2 end)
                   (decf length to-copy)
                   (incf index to-copy)
                   (incf start to-copy)
                   (cond
                     ((= index chunk-size)
                      (funcall encode-fun
                               buffer 0 chunk-size
                               table writer)
                      (setf index 0))
                     (t (return-from #.*stream-write-sequence-function* start)))))
     while (> length chunk-size)
     do (funcall encode-fun seq start (+ start chunk-size) table writer)
       (decf length chunk-size)
       (incf start chunk-size)
     finally
       (unless (plusp length)
         (replace buffer seq :start1 0 :start2 start :end2 end)
         (setf (index stream) length)
         (incf start length))
       (return start)))

(defmethod #.*stream-finish-output-function* ((stream encoding-stream))
  (flush-buffer stream)
  nil)

(defmethod #.*stream-force-output-function* ((stream encoding-stream))
  (flush-buffer stream)
  nil)

(defmethod #.*stream-clear-output-function* ((stream encoding-stream))
  (setf (index stream) 0)
  nil)


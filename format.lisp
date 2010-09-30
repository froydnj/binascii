;;;; format.lisp -- a central repository for encoding formats and accessors

(cl:in-package :binascii)

(defvar *format-descriptors* (make-hash-table))

(defvar *format-state-constructors* (make-hash-table))

(defun unknown-format-error (format)
  (error "Unknown format ~A" format))

(defun find-descriptor-for-format-or-lose (format)
  (or (gethash format *format-descriptors*)
      (unknown-format-error format)))

(defun find-encode-state-constructor-or-lose (format)
  (or (car (gethash format *format-state-constructors*))
      (unknown-format-error format)))

(defun find-decode-state-constructor-or-lose (format)
  (or (cdr (gethash format *format-state-constructors*))
      (unknown-format-error format)))

(defun register-descriptor-and-constructors (format-names
                                             descriptor
                                             encoder-constructor
                                             decoder-constructor)
  (flet ((add-with-specified-format (format)
           (setf (gethash format *format-descriptors*) descriptor)
           (setf (gethash format *format-state-constructors*)
                 (cons encoder-constructor decoder-constructor))))
    (mapc #'add-with-specified-format format-names)
    format-names))

(defmacro define-format (name descriptor-fun encoder-constructor
                         decoder-constructor)
  (let ((*package* (find-package "BINASCII")))
    (let ((binascii-name (intern (symbol-name name)))
          (encode-fun (intern (format nil "ENCODE-~A" name)))
          (decode-fun (intern (format nil "DECODE-~A" name))))
      `(progn
         (export ',encode-fun)
         (defun ,encode-fun (octets &key (start 0) end
                             (element-type 'base-char))
           (encode-to-fresh-vector octets (funcall #',encoder-constructor)
                                   start end element-type))
         (export ',encode-fun)
         (defun ,decode-fun (string &key (start 0) end
                             case-fold map01 decoded-length)
           (decode-to-fresh-vector string (funcall #',decoder-constructor
                                                   case-fold map01)
                                   start end decoded-length))
         (register-descriptor-and-constructors '(,name ,binascii-name)
                                               (,descriptor-fun)
                                               (function ,encoder-constructor)
                                               (function ,decoder-constructor))))))

(defun make-encoder (format)
  "Return an ENCODE-STATE for FORMAT.  Error if FORMAT is not a known
encoding format."
  (let ((constructor (find-encode-state-constructor-or-lose format)))
    (funcall (the function constructor))))

(defun find-encoder (format)
  (etypecase format
    (symbol (make-encoder format))
    (encode-state format)))

(defun make-decoder (format case-fold map01)
  "Return a DECODE-STATE for FORMAT.  Use CASE-FOLD and MAP01 to
parameterize the returned decoder.  Error if FORMAT is not a known
decoding format."
  (let ((constructor (find-decode-state-constructor-or-lose format)))
    (funcall (the function constructor) case-fold map01)))

(defun find-decoder (format case-fold map01)
  (etypecase format
    (symbol (make-decoder format case-fold map01))
    (decode-state format)))

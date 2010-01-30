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

(defun register-descriptor-and-constructors (format
                                             descriptor
                                             encoder-constructor
                                             decoder-constructor)
  (setf (gethash format *format-descriptors*) descriptor)
  (setf (gethash format *format-state-constructors*)
        (cons encoder-constructor decoder-constructor))
  format)

(defun find-encoder (format)
  "Return the appropriate ENCODE-STATE for FORMAT."
  (etypecase format
    (symbol
       (let ((constructor (find-encode-state-constructor-or-lose format)))
         (funcall (the function constructor))))
    (encode-state
       format)))

(defun find-decoder (format case-fold map01)
  "Return the appropriate DECODE-STATE for FORMAT.  If FORMAT is a symbol,
use CASE-FOLD and MAP01 to parameterize the returned decoder."
  (etypecase format
    (symbol
       (let ((constructor (find-decode-state-constructor-or-lose format)))
         (funcall (the function constructor) case-fold map01)))
    (decode-state
       format)))

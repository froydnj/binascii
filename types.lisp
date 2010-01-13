;;;; types.lisp -- various useful types

(cl:in-package :binascii)

(deftype index () '(mod #.array-dimension-limit))

(deftype simple-octet-vector (&optional (length '*))
  #+(or sbcl cmu) `(simple-array (unsigned-byte 8) (,length))
  #-(or sbcl cmu) `(array (unsigned-byte 8) (,length)))

(defun required-argument ()
  (error "Required argument not provided"))

(defstruct (encode-state
             (:copier nil)
             (:constructor))
  (encoded-length (required-argument) :read-only t)
  (octets->string (required-argument) :read-only t)
  (octets->octets (required-argument) :read-only t))

(defstruct (decode-state
             (:copier nil)
             (:constructor))
  (decoded-length (required-argument) :read-only t)
  (string->octets (required-argument) :read-only t)
  (octets->octets (required-argument) :read-only t))

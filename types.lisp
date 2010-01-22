;;;; types.lisp -- various useful types

(cl:in-package :binascii)

(deftype index () '(mod #.array-dimension-limit))

(deftype simple-octet-vector (&optional (length '*))
  #+(or sbcl cmu) `(simple-array (unsigned-byte 8) (,length))
  #-(or sbcl cmu) `(array (unsigned-byte 8) (,length)))

(deftype simple-string ()
  #+sbcl '(and cl:simple-string (not (simple-array nil (*))))
  #+cmu cl:simple-string
  #-(or sbcl cmu) cl:string)

(defun required-argument ()
  (error "Required argument not provided"))

(defstruct (format-descriptor
             (:copier nil)
             (:constructor make-format-descriptor
                           (encoded-length octets->string octets->octets
                                           decoded-length
                                           string->octets
                                           octets->string)))
  (encoded-length (required-argument) :type function :read-only t)
  (octets->string (required-argument) :type function :read-only t)
  (octets->octets (required-argument) :type function :read-only t)
  (decoded-length (required-argument) :type function :read-only t)
  (string->octets (required-argument) :type function :read-only t)
  (octets->octets (required-argument) :type function :read-only t))

(defstruct (encode-state
             (:copier nil)
             (:constructor))
  )

(defstruct (decode-state
             (:copier nil)
             (:constructor))
  )

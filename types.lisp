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
             (:conc-name fd-)
             (:copier nil)
             (:constructor make-format-descriptor
                           (encoded-length octets->string
                                           octets->octets/encode
                                           decoded-length
                                           string->octets
                                           octets->octets/decode)))
  (encoded-length (required-argument) :type function :read-only t)
  (octets->string (required-argument) :type function :read-only t)
  (octets->octets/encode (required-argument) :type function :read-only t)
  (decoded-length (required-argument) :type function :read-only t)
  (string->octets (required-argument) :type function :read-only t)
  (octets->octets/decode (required-argument) :type function :read-only t))

(defstruct (state
             (:copier nil)
             (:predicate nil)
             (:constructor nil))
  (descriptor (required-argument) :type format-descriptor :read-only t)
  ;; FINISHED-INPUT-P is either T or NIL depending on whether we have
  ;; seen all of the input.
  (finished-input-p nil)
  ;; Likewise for FINISHED-OUTPUT-P.
  (finished-output-p nil))

(defstruct (encode-state
             (:include state)
             (:copier nil)
             (:predicate nil)
             (:constructor nil))
  ;; LINE-BREAK describes after how many characters we should be
  ;; inserting newlines into the encoded output.  It is zero if we
  ;; should never insert newlines.
  (line-break 0 :type (integer 0 *)))

(defstruct (decode-state
             (:include state)
             (:copier nil)
             (:predicate nil)
             (:constructor nil))
  )

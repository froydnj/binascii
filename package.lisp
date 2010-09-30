;;;; package.lisp -- package definition for BINASCII

(cl:defpackage :binascii
  (:use :cl)
  (:shadow simple-string)

  ;; Main entry points for one-shot encoding and decoding.
  (:export #:encode #:decode)

  ;; Incremental encoding and decoding.
  (:export #:encode-octets #:decode-octets
           #:make-encoder #:make-decoder)

  ;; Format names.  For each NAME here, there exists a ENCODE-NAME and
  ;; DECODE-NAME function that work identically to ENCODE and DECODE,
  ;; respectively.
  (:export #:base16 #:hex #:base32 #:base32hex
           #:base64 #:base64url #:base85 #:ascii85))

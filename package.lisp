;;;; package.lisp -- package definition for BINASCII

(cl:defpackage :binascii
  (:use :cl)
  (:shadow simple-string)
  (:export
   #:encode-octets #:encode
   #:decode-octets #:decode

   ;; Format names.
   #:base16
   #:hex
   #:base32
   #:base32hex
   #:base64
   #:base64url
   #:base85
   #:ascii85
   ))

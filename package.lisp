;;;; package.lisp -- package definition for BINASCII

(cl:defpackage :binascii
  (:use :cl)
  (:shadow simple-string)
  (:export
   #:encode-octets #:encode
   #:decode-octets #:decode
   ))

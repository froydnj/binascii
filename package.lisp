;;;; package.lisp -- package definition for BINASCII

(cl:defpackage :binascii
  (:use :cl)
  (:export #:encode-octets #:decode-octets))

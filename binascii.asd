; -*- mode: lisp -*-

(cl:defpackage #:binascii-system
  (:use :cl :asdf :uiop))

(cl:in-package #:binascii-system)

(defsystem "binascii"
  :version "1.0"
  :author "Nathan Froyd <froydnj@gmail.com>"
  :maintainer "Nathan Froyd <froydnj@gmail.com>"
  :description "A library of ASCII encoding schemes for binary data"
  :license "BSD-style (http://opensource.org/licenses/BSD-3-Clause)"
  :components ((:static-file "LICENSE")
               (:file "package")
               (:file "types" :depends-on ("package"))
               (:file "format" :depends-on ("types"))
               (:file "octets" :depends-on ("types" "format"))
               (:file "ascii85" :depends-on ("octets"))
               (:file "base85" :depends-on ("octets"))
               (:file "base64" :depends-on ("octets"))
               (:file "base32" :depends-on ("octets"))
               (:file "base16" :depends-on ("octets")))
  :in-order-to ((test-op (test-op "binascii/tests"))))

(defclass test-vector-file (static-file)
  ((type :initform "testvec")))

(defsystem "binascii/tests"
  :depends-on ("binascii")
  :version "1.0"
  :components ((:module "tests"
                        :components
                        ((:file "rt")
                         (:file "tests" :depends-on ("rt"))
                         (:test-vector-file "ascii85")
                         (:test-vector-file "base85")
                         (:test-vector-file "base64")
                         (:test-vector-file "base32")
                         (:test-vector-file "base32hex")
                         (:test-vector-file "base16"))))
  :perform (test-op (o c)
             (or (symbol-call :rtest :do-tests)
                 (error "TEST-OP failed for BINASCII/TESTS"))))

; -*- mode: lisp -*-

(cl:defpackage #:binascii-system
  (:use :cl :asdf))

(cl:in-package #:binascii-system)

(asdf:defsystem :binascii
  :version "1.0"
  :author "Nathan Froyd <froydnj@gmail.com>"
  :maintainer "Nathan Froyd <froydnj@gmail.com>"
  :description "A library of ASCII encoding schemes for binary data"
  :components ((:file "package")
               (:file "octets" :depends-on ("package"))
               (:file "base85" :depends-on ("octets"))
               (:file "base64" :depends-on ("octets"))
               (:file "base32" :depends-on ("octets"))
               (:file "base16" :depends-on ("octets"))))

(defmethod perform ((op test-op) (c (eql (find-system :binascii))))
  (asdf:oos 'asdf:test-op :binascii-tests))

(defmethod operation-done-p ((op test-op) (c (eql (find-system :binascii))))
  nil)

(defclass test-vector-file (static-file)
  ())

(defmethod source-file-type ((c test-vector-file) (s module)) "testvec")

(asdf:defsystem :binascii-tests
  :depends-on (binascii)
  :version "1.0"
  :in-order-to ((test-op (load-op :binascii-tests)))
  :components ((:module "tests"
                        :components
                        ((:file "rt")
                         (:file "tests")
                         (:test-vector-file "base85")
                         (:test-vector-file "base64")
                         (:test-vector-file "base32")
                         (:test-vector-file "base32hex")
                         (:test-vector-file "base16")))))

(defmethod operation-done-p ((op test-op) (c (eql (find-system :binascii-tests))))
  nil)

(defmethod perform ((op test-op) (c (eql (find-system :binascii-tests))))
  (or (funcall (intern "DO-TESTS" (find-package "RTEST")))
      (error "TEST-OP failed for BINASCII-TESTS")))

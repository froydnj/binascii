;;;; types.lisp -- various useful types

(cl:in-package :binascii)

(deftype index () '(mod #.array-dimension-limit))

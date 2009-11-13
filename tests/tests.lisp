(cl:defpackage #:binascii-tests
  (:use :cl))

(cl:in-package #:binascii-tests)

(defun ascii-string-to-octets (string &key (start 0) end)
  "Convert STRING to a (VECTOR (UNSIGNED-BYTE 8)).  It is an error if
STRING contains any character whose CHAR-CODE is greater than 255."
  (declare (type string string)
           (type fixnum start)
           (type (or cl:null fixnum) end)
           (optimize (speed 3) (safety 1)))
  (let* ((length (length string))
         (vec (make-array length :element-type '(unsigned-byte 8)))
         (end (or end length)))
    (loop for i from start below end do
          (let ((byte (char-code (char string i))))
            (unless (< byte 256)
              (error "~A is not an ASCII character" (char string i)))
            (setf (aref vec i) byte))
          finally (return vec))))

(defun test-vector-filename (ident)
  (merge-pathnames (make-pathname :directory '(:relative)
                                  :name (format nil "~(~A~)" ident)
                                  :type "testvec")
                   #.*compile-file-pathname*))

(defun run-test-vector-file (name function-map)
  (let ((filename (test-vector-filename name))
        (*readtable* (copy-readtable)))
    (with-open-file (stream filename :direction :input
                            :element-type 'character
                            :if-does-not-exist :error)
      (loop for form = (read stream nil stream)
         until (eq form stream) do
         (cond
           ((not (listp form))
            (error "Invalid form in test vector file ~A: ~A" filename form))
           (t
            (let ((test-function (cdr (assoc (car form) function-map))))
              (unless test-function
                (error "No test function defined for ~A" (car form)))
              (apply test-function name (cdr form)))))
         finally (return t)))))

(defun encoding-test* (name input encoded-output decoded-length)
  (let ((output (binascii:encode-octets nil input name :end decoded-length))
        (decoded-input (binascii:decode-octets nil encoded-output name
                                               :decoded-length decoded-length)))
    (when (mismatch output encoded-output)
      (error "encoding ~A failed on ~A, produced ~A, wanted ~A"
             name input output encoded-output))
    (when (mismatch input decoded-input :end1 decoded-length :end2 decoded-length)
      (error "decoding ~A failed on ~A, produced ~A, wanted ~A"
             name encoded-output decoded-input input))))

(defun encoding-test (name hexinput encoded-output &optional decoded-length)
  (encoding-test* name (binascii:decode-octets nil hexinput :hex)
                  encoded-output decoded-length))

(defun encoding-test-ascii (name ascii-input encoded-output
                            &optional decoded-length)
  (encoding-test* name (ascii-string-to-octets ascii-input)
                  encoded-output decoded-length))

(defparameter *encoding-tests*
  (list (cons :encoding-test 'encoding-test)
        (cons :encoding-ascii-test 'encoding-test-ascii)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *encodings* '(:base16 :base32 :base32hex :base64 :base85)))

#.(loop for e in *encodings*
     collect `(rtest:deftest ,(intern (format nil "~A/TO-NIL/BASE-CHAR" e))
                  (run-test-vector-file ,e *encoding-tests*)
                t) into forms
     finally (return `(progn ,@forms)))

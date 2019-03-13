
(in-package :cl-user)
(defpackage regex.test
  (:use :cl)
  (:nicknames :re-test)
  (:import-from :regex :scan :compile-regex)
  (:export :run-test))

(in-package :regex.test)

(defmacro print-multi (values)
  `(format t "~{~a ~}~%" (multiple-value-list ,values)))

(defun do-test (tests)
  (loop :for (regex target expected) in tests
     :do (format t "regexing ~a from ~a to get ~a~%" regex target expected)
     :do (print-multi (re:scan regex target))))

(defvar *tests* nil)

(defmacro add-test (&rest rest)
  `(push (list ,@rest) *tests*))

;; (add-test "ab?c" "ac" "ac")
(add-test "ab?c" "ac" "ac")
(add-test "abc?" "abcdefgh" "abc")
(add-test "a+[ab]+" "aabbaacc" "aabbaa")
(add-test "aa" "aabbaacc" "aa")

(defun run-test ()
  ;; (do-test *tests*)
  (let ((compiled (re:compile-regex "a+b*.*$"))
      (ls (uiop:read-file-lines "~/.local/quicklisp/local-projects/cl-regex/t/test.txt")))
    (loop :for i in ls
       :if (> (length i) 0)
       :collect (re:scan compiled i))))

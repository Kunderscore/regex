#|
Copyright (c) 2015, asciian.

This program is free software; you can redistribute it and/or modify
it under the terms of the Lisp Lesser General Public License version 2, as published by
the Free Software Foundation and with the following preamble:
http://opensource.franz.com/preamble.html

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Lisp Lesser General Public License for more details.
|#
(in-package :cl-user)
(defpackage regex.parser
  (:use :cl :regex.core)
  (:export :parse))
(in-package regex.parser)

(defun syntex-check (str)
  (loop :with p1 := 0 :with p2 := 0 :with p3 := 0
	:with s := (make-string-input-stream str)
	:for  c := (read-char s nil nil) :while c :do
	  (cond ((char= c #\() (incf p1))
		((char= c #\)) (decf p1))
		((char= c #\[) (incf p2))
		((char= c #\]) (decf p2))
		((char= c #\{) (incf p3))
		((char= c #\}) (decf p3))
		((char= c #\\) (read-char s nil nil)))
	:finally
	   (unless (and (zerop p1) (zerop p2) (zerop p3))
	     (error "Invalid regular expression"))))

(defmacro expand-base (name regexes &body body)
  `(let (start end ls)
     (setf (values start end ls)
           (match ,regexes str 0))
     (unless start (return-from ,name str))
     (concatenate 'string
                  (subseq str 0 start)
                  ,@body
                  (subseq str end))))

(defun expand-a-z-1 (str)
  (expand-base
   expand-a-z-1
   (list
    (re/group (list (re/dot)))
	(re/character #\-)
	(re/group (list (re/dot)))
	(re/ok))
   (loop :for c :from (char-int (char (first ls) 0))
	  :to (char-int (char (second ls) 0))
	  :collect (code-char c))))

(defun expand-a-z (str)
  (if (position #\- str :test 'char=)
      (loop :repeat (count #\- str)
	     :for s := (expand-a-z-1 str) :then (expand-a-z-1 s)
	     :finally (return s))
      str))

(defun expand-class (str)
  (expand-base
   expand-class
   (list
    (re/character #\:)
	(re/group
	 (list
	  (re/plus
	   (re/brackets
		(expand-a-z "a-z")))))
	(re/character #\:)
	(re/ok))
   (cond
     ((string= "upper" (first ls))(expand-a-z "A-Z"))
	 ((string= "lower" (first ls))(expand-a-z "a-z"))
	 ((string= "alpha" (first ls))(expand-a-z "a-zA-Z"))
	 ((string= "alnum" (first ls))(expand-a-z "0-9a-zA-Z"))
	 ((string= "digit" (first ls))(expand-a-z "0-9"))
	 ((string= "xdigit"(first ls))(expand-a-z "0-9A-Fa-f"))
	 ((string= "punct" (first ls))
	  "\\]\\[!\"#$%&'()*+,./:;<=>?@\\^_`{|}~-")
	 ((string= "blank" (first ls)) '(#\space #\tab)))))

(defun parse-pairs (s start-char end-char)
  (loop :with cnt := 1
	 :for i :from 0
	 :for c := (read-char s nil nil)
	 :until (and (= cnt 1) (char= c end-char))
	 :if (char= c #\\)
     :do (setf c (read-char s nil nil))
     :else :if (char= c start-char)
     :do (incf cnt)
     :else :if (char= c end-char)
     :do (decf cnt)
     :collect c :into ret
	 :finally
       (return (coerce ret 'string))))

(defun parse-parenthesis (s)
  (let ((str (parse-pairs s #\( #\))))
    (re/group
     (parse-1
	  (make-string-input-stream
	   str)))))

(defun parse-brackets (s)
  (let* ((str (parse-pairs s #\[ #\]))
         (inver (char= (elt str 0) #\^)))
    (if inver
        (setf str (subseq str 1)))
    (re/brackets
     (expand-a-z (expand-class str))
     inver)))

(defun parse-brace (s)
  (let ((re
         (list
          (re/group (list (re/plus (re/brackets "0123456789"))))
          (re/character #\,)
          (re/group (list (re/star (re/brackets "0123456789"))))
          (re/ok)))
	    (str
         (loop :for c := (read-char s nil nil) :until (char= c #\})
		    :collect c :into u
		    :finally (return (coerce u 'string)))))
    (multiple-value-bind (start end ls) (match re str 0)
      (declare (ignore end))
      (if start
	      (if (string= "" (second ls))
	          (lambda (r) (re/repeat (read-from-string (first ls)) most-positive-fixnum r))
	          (lambda (r) (re/repeat (read-from-string (first ls)) (read-from-string (second ls)) r)))
	      (lambda (r) (re/repeat (read-from-string str) (read-from-string str) r))))))

(defun parse-1 (s)
  (loop :with ast := nil
	 :for  c   := (read-char s nil nil) :while c :do
	   (cond ((char= c #\[) (push (parse-brackets s) ast))
		     ((char= c #\{) (push (funcall (parse-brace s) (pop ast)) ast))
		     ((char= c #\() (push (parse-parenthesis s) ast))
		     ((char= c #\.) (push (re/dot) ast))
		     ((char= c #\*) (push (re/star (pop ast)) ast))
		     ((char= c #\+) (push (re/plus (pop ast)) ast))
		     ((char= c #\?) (push (re/question (pop ast)) ast))
		     ((char= c #\$) (push (re/tail) ast))
		     ((char= c #\^) (push (re/head) ast))
		     ((char= c #\\)
		      (let ((e (read-char s nil nil)))
		        (if (member e (coerce "0123456789" 'list))
			        (push (re/ref (- (char-int e) (char-int #\0))) ast)
			        (push (re/character e) ast))))
		     ((char= c #\|) (setf ast (list (re/or
                                             (reverse ast) (parse-1 s)))))
		     (t (push (re/character c) ast)))
	 :finally (return (reverse ast))))

(defun parse (str)
  (syntex-check str)
  (append (parse-1 (make-string-input-stream str)) (list (re/ok))))

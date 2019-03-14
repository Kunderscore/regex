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

(defpackage regex.core
  (:use :cl)
  (:export
   :re/character
   :re/dot
   :re/question
   :re/star
   :re/group
   :re/head
   :re/tail
   :re/plus
   :re/brackets
   :re/brackets-not
   :re/ok
   :re/repeat
   :re/ref
   :re/or
   :match
   :match-all))
(in-package :regex.core)

(defstruct
    (data
      (:constructor create-data
		            (string &aux (length (length string)))))
  (register nil :type list)
  (index    0   :type fixnum)
  (start    0   :type fixnum)
  (length   0   :type fixnum :read-only t)
  (string   ""  :type string :read-only t))

(declaim (inline sar))
(defun sar (data)
  (declare (type data data))
  (when (< (data-index data) (data-length data))
    (let ((c (char (data-string data) (data-index data))))
      (unless (char= #\Newline c) c))))

(declaim (inline sdr))
(defun sdr (data)
  (declare (type data data))
  (incf (data-index data)))

(defmacro push-curnt-str (data)
  `(push (subseq (data-string ,data) (data-start ,data) (data-index ,data))
         (data-register ,data)))

(defmacro defmatcher (name args body &optional &key (progress nil))
  `(defun ,name ,args
     (lambda (data)
         (declare (ignorable data)
		          (type data data))
         ,(if progress
              `(let ((result ,body))
                 (if result
                     (sdr data))
                 result)
              `,body))))

(defmatcher re/character (c)
  (eql c (sar data))
  :progress t)

(defmatcher re/dot ()
  (sar data)
  :progress t)

(defmatcher re/repeat (n m r)
  (let ((start (data-index data)))
    (if (and (= n 0) (= m 1))
        (funcall r data)
        (loop
           :while (and
                   (< (data-index data) (length (data-string data)))
                   (funcall r data))))
    (if (>= (- (data-index data) start) n)
        t)))

(defun re/question (r)
  (re/repeat 0 1 r))

(defun re/star (r)
  (re/repeat 0 -1 r))

(defun re/plus (r)
  (re/repeat 1 -1 r))

(defmatcher re/or (r1 r2)
  (some (lambda (regs)
          (not (notevery (lambda (x) (funcall x data)) regs)))
        (list r1 r2)))

(defmatcher re/head ()
  (= (data-start data) (data-index data) 0))

(defmatcher re/tail ()
  (unless (< (data-index data) (data-length data))
    (throw 'match
      (values
       (data-start data)
       (data-index data)
       (reverse (data-register data))))))

(defmatcher re/ref (n)
  (let*
      ((str (nth (1- n) (reverse (data-register data))))
	   (regs (map 'list #'re/character str)))
    (every (lambda (x) (funcall x data)) regs)))

(defmatcher re/group (r)
  (let ((start (data-index data)))
    (if r
        (if (every (lambda (x) (funcall x data)) r)
            (push
             (subseq (data-string data) start (data-index data))
             (data-register data)))
        t)))

(defmatcher re/brackets (regex &optional (exclude nil))
  (let ((match (position (sar data) regex)))
    (if exclude
        (not match)
        match))
  :progress t)

(defmatcher re/ok ()
  (throw 'match
    (let ((start (data-start data))
          (index (data-index data)))
      (values
       start
       index
       (reverse (data-register data))
       (subseq (data-string data) start index)))))

(defun match (reg str start &optional &key (end most-positive-fixnum) data)
  (if (not (data-p data))
      (setf data (create-data str)))
  (catch 'match
    (loop
       :for i :from start :to (min end (data-length data))
       :do (setf (data-start data) i
                 (data-index data) i)
       :do
         ;; (every (lambda (r) (funcall r data)) reg)
         (loop :named check-loop
            :for r in reg
            :unless (funcall r data)
            :do (return-from check-loop)))))

(defun match-all (reg str)
  (loop :with data = (create-data str)
     :for start = 0 :then (second values)
     :for values = (multiple-value-list (match reg str start :data data))
     :while (first values)
     :collect values
     :do (setf (data-register data) nil)))
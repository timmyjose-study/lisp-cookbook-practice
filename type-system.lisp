;;;; The Type System

(defun plus1 (arg)
  (typecase arg
    (integer (1+ arg))
    (string (concatenate 'string arg "1"))
    (t 'error)))

;; defining a new type

(deftype small-number-array (&optional type)
  `(and (array ,type 1)
        (satisfies small-number-array-p)))

(defun small-number-array-p (thing)
  (and (arrayp thing)
       (<= (length thing) 10)
       (every #'numberp thing)
       (every #'(lambda (n) (< n 10)) thing)))

(defun sub1 (arg)
  (check-type arg number)
  (1- arge))

;; declaim

(declaim (type (string) *name*))
(defparameter *name* "book")

;; this gives a type error at compile time
;; (setf *name* :me)

(deftype list-of-strings ()
  `(satisfies list-of-strings-p))

(defun list-of-strings-p (thing)
  (and (listp thing)
       (every #'stringp thing)))

(declaim (type (or null list-of-strings) *foo*))
(defparameter *foo* nil)

(setf *foo* (list "hello" "world"))

;; error at compile time
;;(setf *foo* :hello)

(declaim (ftype (function (fixnum) fixnum) add))
(defun add (n)
  (1+ n))

(declaim (ftype (function (fixnum fixnum) fixnum) mult))
(defun mult (x y)
  (* x y))

;; compile-time type-checking of slots

(defclass quux ()
  ((name :type number :initform "hello")))

;; using serapeum

(serapeum:-> add-three (fixnum fixnum fixnum) fixnum)
(defun add-three (a b c)
  (+ a b c))



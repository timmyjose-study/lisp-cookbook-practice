;;;; Pattern matching using trivia.

(use-package :trivia)

(match '(1 2 3)
  ((cons x y)
   (print x)
   (print y)))

(match '(1 2 3)
  ((list a b c)
   (print a)
   (print b)
   (print c)))

(multiple-value-bind (x y) (match '(something 2 3)
                             ((list a b _)
                              (values a b)))
  (list :x x :y y))

(assert (null (match '(something 2 3)
                ((list a b)
                 (values a b)))))

(multiple-value-bind (x y) (match '(something 2 3)
                             ((list* a b)
                              (values a b)))
  (list :x x :y y))

(assert (= 3 (match '(1 2 . 3)
             ((list* _ _ c)
              c))))

;; but note
(match #(0 1 2)
  ((list* a)
   a))

(assert (= 1 (match #(0 1 2)
               ((vector _ b _ )
                b))))

(assert (null (match #(0 1 2)
                ((vector _ b)
                 b))))

(multiple-value-bind (x z) (match #(0 1 2 3)
                             ((vector* x _ z)
                              (values x z)))
  (list :x x :z z))

;; structs

(defstruct foo bar baz)
(defvar *x* (make-foo :bar 100 :baz 200))

;; make-instance style
(multiple-value-bind (bar baz) (match *x*
                                 ((foo :bar bar :baz baz)
                                  (values bar baz)))
  (format t "bar = ~d, baz = ~d~%" bar baz))

;; with-slots style
(multiple-value-bind (bar baz) (match *x*
                                 ((foo (bar bar) (baz baz))
                                  (values bar baz)))
  (format t "bar = ~d, baz = ~d~%" bar baz))

;; slot name style

(multiple-value-bind (bar baz) (match *x*
                                 ((foo bar baz)
                                  (values bar baz)))
  (format t "bar = ~d, baz = ~d~%" bar baz))

;; classes

(defclass employee ()
  ((name
    :initarg :name
    :accessor name)
   (age
    :initarg :age
    :initform 0
    :accessor age)
   (salary
    :initarg :salary
    :initform 0.0
    :accessor salary)))

(defmethod print-object ((obj employee) stream)
  (print-unreadable-object (obj stream :type nil :identity nil)
    (with-accessors ((name name) (age age) (salary salary)) obj
      (with-standard-io-syntax
        (format stream "Employee{ name: ~a, age: ~d, salary: ~,2f }" name age salary)))))

(defun make-employee (&key name age salary)
  (make-instance 'employee :name name :age age :salary salary))

(multiple-value-bind (name salary) (match (make-employee :name "Bob" :salary 123.45 :age 42)
                                     ((employee (name name) (salary salary))
                                      (values name salary)))
  (format t "Name = ~a, salary = ~,2f" name salary))

;; conditional patterns

(assert (= 1 (match (list 1 2)
               ((or (list a _)
                    (cons _ a))
                a))))
(assert (= 1 (match (cons 2 1)
               ((or (list a _)
                    (cons _ a))
                a))))

;; guards

(multiple-value-bind (a b) (match (list 2 5)
                             ((guard (list x y)
                                     (= 10 (* x y)))
                              (values x y)))
  (and
   (assert (= 2 a))
   (assert (= 5 b))))

;; nested patterns

(multiple-value-bind (x y z) (match (list 1 (list 2 3) 4 5 6)
                               ((list* x (list _ y) _ z _)
                                (values x y z)))
  (format t "x = ~d, y = ~d, z = ~d~%" x y z))
g

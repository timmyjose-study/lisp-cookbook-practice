;;;; Data Structures

(setf *print-circle* t)

(defun circular! (items)
  "Modifies the last cdr if list items, returning a circular list"
  (setf (cdr (last items)) items))

(destructuring-bind (x y z) (list 1 2 3)
  (and
   (assert (eq 1 x))
   (assert (eq 2 y))
   (assert (eq 3 z))))

(destructuring-bind (x (y1 &optional y2) z) (list 1 (list 2 20) 3)
  (and
   (assert (eq 1 x))
   (assert (eq 2 y1))
   (assert (eq 20 y2))
   (assert (eq 3 z))))

(destructuring-bind (x (y1 &optional y2) z) (list 1 (list 2) 3)
  (and
   (assert (eq 1 x))
   (assert (eq 2 y1))
   (assert (null y2))
   (assert (eq 3 z))))

(destructuring-bind (&whole whole-list &key x y z) (list :z 3 :x 1 :y 2)
  (and
   (assert (equal '(:z 3 :x 1 :y 2) whole-list))
   (assert (eq 1 x))
   (assert (eq 2 y))
   (assert (eq 3 z))))

(destructuring-bind (&key a (b :not-found) c &allow-other-keys)
    '(:c 23 :a #\A :d "D" :foo :bar :baz :quux)
  (list a b c))

(defparameter some-list (make-list 10))

(fill some-list "hello")


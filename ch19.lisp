(defpackage :ch19
  (:use :common-lisp))
(in-package :ch19)

(defun natural-numbers-from (n)
  "Returns an infinite list of the natural numbers
   starting from N using the lazy evaluation technique"
  (check-type n integer)
  `(,n :promise natural-numbers-from ,(1+ n)))

(defun lazy-first (list)
  "Returns the first member of LIST, redeeming a promise if necessary"
  (check-type list list)
  (if (eql (first list) :promise)
      (first (eval (rest list)))
      (first list)))

(defun lazy-rest (list)
  "Returns the rest of the LIST, redeeming a promise if necessary"
  (check-type list list)
  (if (eql (first list) :promise)
      (rest (eval (rest list)))
      (rest list)))

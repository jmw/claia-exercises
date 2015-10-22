(defpackage :ch17
  (:use :common-lisp))
(in-package :ch17)

(defun mycopy (l)
  "Returns a copy of list l"
  (check-type l list)
  (if (null l) '()
      (cons (first l) (mycopy (rest l)))))

(defun myappend (l1 l2)
  "Returns a list consisting of the members of l1
followed by the members of l2"
  (check-type l1 list)
  (check-type l2 list)
  (if (null l1) l2
      (cons (first l1) (myappend (rest l1) l2))))

(defun myreverse (l)
  "Returns a copy of the list L with the order of the members reversed."
  (check-type l list)
  (if (null l) '()
      (append (myreverse (rest l))
              (list (first l)))))

(defun reverse2 (l1 l2)
  "Returns a list consisting of the members of L1 in reverse order
followed by the members of L2 in original order."
  (check-type l1 list)
  (check-type l2 list)
  (if (null l1) l2
      (reverse2 (rest l1)
                (cons (first l1) l2))))

(defun reverse1 (l)
  "Returns a copy of the list L with the order of members reversed"
  (check-type l list)
  (reverse2 l '()))

;;(defun sub-first (new old l)
;;  "Returns a copy of the list L with the element NEW replacing the first occurrence of the element OLD"
;;  (check-type new (satisfies util:elementp))
;;  (check-type old (satisfies util:elementp))
;;  (check-type l list)
;;  (cond ((null l) '())
;;        ((eql (first l) old) (cons new (rest l)))
;;        (t (cons (first l)
;;                 (sub-first new old (rest l))))))

(deftype element () "Elements are objects testable by EQL,
namely symbols, characters, numbers, and packages."
         '(satisfies util:elementp))

(defun sub-first (new old l)
  "Returns a copy of the list L with the element NEW replacing the first occurrence of the element OLD"
  (check-type new element)
  (check-type old element)
  (check-type l list)
  (cond ((null l) '())
        ((eql (first l) old) (cons new (rest l)))
        (t (cons (first l)
                 (sub-first new old (rest l))))))

(defun makeset (b)
  "Returns a set containing just those elements of the input bag B."
  (check-type b list)
  (cond ((null b) '())
        ((member (first b) (rest b))
         (makeset (rest b)))
        (t (cons (first b) (makeset (rest b))))))


(defun identity (object)
  "Returns its argument unmodified"
  object)

(defun firstn (n l)
  "Returns a list whose members are the first n members of l."
  (check-type l list)
  (check-type n number)
  (cond ((null l) nil)
        ((= n 0) '())
        (t (cons (first l) (firstn (1- n) (rest l))))))

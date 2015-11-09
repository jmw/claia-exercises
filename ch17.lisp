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

(defun subst* (new old l)
  "Returns a copy of the list L with the element NEW replacing the all top-level occurrences of the element OLD"
  (check-type new element)
  (check-type old element)
  (check-type l list)
  (cond ((null l) '())
        ((eql (first l) old) (cons new (subst* new old (rest l))))
        (t (cons (first l)
                 (subst* new old (rest l))))))

;; 17.28 The cross product of two sets s1 and s2 is the set s3, which consists
;; of all pairs such that the first of each pair is a member of s1 and the second
;; of each pair is a member of s2.  In the ch17 package, define (xprod s1 s2) to return
;; the cross product of the sets s1 and s2.  For example,
;;     (xprod '(:set a b) '(:set c d e))
;; should evaluate to
;;     (:set (ac) (ad) (ae) (bc) (bd) (be))
;; or any other ordering of these six pairs.  Where appropriate, use the
;; functions whose names are external symbols in the set package,
;; but do not use any function whose name is an internal symbol in the set package.
;; Hint: use a helper function xprod1 that takes an object and a set and returns
;; a list of pairs.  For example,
;;     (xprod1 'a '(:set c d e))
;; would return
;;   (:set (a c) (a d) (a e))

(defun xprod1 (elt s1)
  (set:makeset (xprod1-unlabeled elt (rest s1))))

;; ok:
;; (xprod1-unlabeled 'a '(c d e))
;; ((A C) (A D) (A E))

(defun xprod1-unlabeled (elt s1)
  (cond ((null s1) nil)
        (t (cons (list elt (car s1)) (xprod1-unlabeled elt (cdr s1))))))

;; returns one list too many:
;; (xprod-unlabeled '(a b) '(c d e))
;; (((A C) (A D) (A E)) ((B C) (B D) (B E)))

(defun xprod-unlabeled (s1 s2)
  (cond ((null s2) nil)
        ((null s1) nil)
        (t (cons (xprod1-unlabeled (car s1) s2) (xprod-unlabeled (cdr s1) s2)))))

;; Almost: see problem with xprod1-unlabeled
;; (xprod '(:set a b) '(:set c d e))
;; (:SET ((A C) (A D) (A E)) ((B C) (B D) (B E)))
(defun xprod (s1 s2)
  (set:makeset (xprod-unlabeled (cdr s1) (cdr s2))))


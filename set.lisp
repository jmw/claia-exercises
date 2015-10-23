(defpackage :set 
  (:use :common-lisp :util)
  (:export :setp :set :makeset))

(in-package :set)

(shadow 'common-lisp:set)
(shadow 'common-lisp:union)

(defun setp (l)
  (cond ((not (listp l)) nil)
        ((null l) t)
        ((member (car l) (cdr l)) nil)
        (t (setp (cdr l)))))

(deftype set ()
  "A set is a list of objects, no two of which are eql"
         '(satisfies set:setp))

(defun makeset (b)
  "Returns a set containing just those elements of the input bag B."
  (check-type b bag)
  (cond ((null b) '())
        ((member (first b) (rest b))
         (makeset (rest b)))
        (t (cons (first b) (makeset (rest b))))))

(defun union (s1 s2)
  "Returns the union of the sets S1 and S2."
  (check-type s1 set)
  (check-type s2 set)
  (cond ((null s1) s2)
        ((member (first s1) s2)
         (union (rest s1) s2))
        (t (cons (first s1) (union (rest s1) s2)))))


;; 17.18 Redefine makeset in the set file so that every set is represented by
;; a list whose first member is :set
(defun makeset1 (b)
  "Returns a set containing just those elements of the input bag B."
  (check-type b bag)
  (cond ((null b) '())
        ((member (first b) (rest b))
         (makeset1 (rest b)))
        (t (cons (first b) (makeset1 (rest b))))))

(defun makeset (b)
  "Returns a set containing just those elements of the input bag B, with :set as the first element."
  (check-type b bag)
  (cond ((null b) nil)
        (t (cons :set (makeset1 b)))))

;; 17.19 Redefine setp in the set file so that it just checks that the first
;; member of a list that represents a set is :set
(defun setp (l)
  (cond ((not (listp l)) nil)
        ((equal (first l) :set) t)
        (t nil)))

;; 17.20 Define set:first and set:rest in your set file to return the element
;; that happens to be listed first in a set, and the set without that element, respectively.
;; Make these external symbols in the set package and shadow common-lisp:first and
;; common-lisp:rest.  Go through all the definitions in your set file; make sure to type
;; common-lisp:first and common-lisp:rest wherever necessary.

(defpackage :set 
  (:use :common-lisp :util)
  (:export :setp :set :makeset :union :first :rest :insert :empty :intersection :complement :subsetp :equal))

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
  (check-type b util:bag)
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
  (check-type b util:bag)
  (cond ((null b) '())
        ((member (first b) (rest b))
         (makeset1 (rest b)))
        (t (cons (first b) (makeset1 (rest b))))))

(defun makeset (b)
  "Returns a set containing just those elements of the input bag B, with :set as the first element."
  (check-type b util:bag)
  (cond ((null b) nil)
        (t (cons :set (makeset1 b)))))

;; 17.19 Redefine setp in the set file so that it just checks that the first
;; member of a list that represents a set is :set
(defun setp (l)
  "Returns true if L is a list which begins with the keyword :set."
  (cond ((not (listp l)) nil)
        ((equal (common-lisp:first l) :set) t)
        (t nil)))

;; or more concisely:
(defun setp (l)
  "Returns true if L is a list which begins with the keyword :set."
  (and (listp l)
       (equal (common-lisp:first l) :set)))

;; 17.20 Define set:first and set:rest in your set file to return the element
;; that happens to be listed first in a set, and the set without that element, respectively.
;; Make these external symbols in the set package and shadow common-lisp:first and
;; common-lisp:rest.  Go through all the definitions in your set file; make sure to type
;; common-lisp:first and common-lisp:rest wherever necessary.
(shadow 'common-lisp:first)
(defun first (s)
  "Return the first element in set S"
  (check-type s set)
  (common-lisp:first (common-lisp:rest s)))

(shadow 'common-lisp:rest)
(defun rest (s)
  "Return the set S without the first element"
  (check-type s set)
  (common-lisp:rest (common-lisp:rest s)))

;; 17.21 Define (insert e s) to return a set just like s, but
;; with e added as an additional element.  If e is already in s, s should
;; be returned unchanged.  Make insert an external symbol in the set package.
(defun insert (e s)
  "Returns a set just like s, but with e added as an additional element. If e is already in s, s is returned unchanged."
  (check-type s set:set)
  (cond ((member e (rest s)) s)
        (t (cons :set (cons e (rest s))))))

;; 17.22 Define empty to be a function that returns true
;; if its one argument is a set with no elements, and false otherwise.
;; Make empty an external symbol in the set package
(defun empty (l)
  "Returns true if its one argument is a set with no elements, and false otherwise."
  (cond ((and (setp l) (null (common-lisp:rest l))) t)
        (t nil)))

;; 17.23 Redefine set:union to use the new representation of sets.
;; Define the helper function union-unlabeled-sets as an internal function.
;; Again evaluate (union '(a b c d) '(b d e f)) while tracing setp.
(defun union-unlabeled-sets (s1 s2)
(cond ((null s1) s2)
      ((member (common-lisp:first s1) s2) (union-unlabeled-sets (common-lisp:rest s1) s2))
      (t (cons (common-lisp:first s1) (union-unlabeled-sets (common-lisp:rest s1) s2)))))

(defun union (s1 s2)
  "Returns the union of the sets S1 and S2."
  (check-type s1 set)
  (check-type s2 set)
  (union-unlabeled-sets (common-lisp:rest s1) (common-lisp:rest s2)))


;; 17.24 The intersection of two sets s1 and s2 is the set consisting
;; of those elements that are in s1 and also in s2.  Define intersection
;; in your set file, with intersection an external symbol in the set package.
;; Shadow lisp:intersection.
(shadow 'common-lisp:intersection)

(defun intersection-unlabeled-sets (s1 s2)
  (cond ((or (null s1) (null s2)) nil)
        ((not (member (common-lisp:first s1) s2)) (intersection-unlabeled-sets (common-lisp:rest s1) s2))
        (t (cons (common-lisp:first s1) (intersection-unlabeled-sets (common-lisp:rest s1) s2)))))

(defun intersection (s1 s2)
  "Returns the intersection of sets S1 and S2."
  (check-type s1 set)
  (check-type s2 set)
  (makeset (intersection-unlabeled-sets (common-lisp:rest s1) (common-lisp:rest s2))))


;; 17.25 The relative complement of two sets s1 and s2 is
;; the set consisting of those elements of s1 that are not also in s2.
;; Define complement in your set file, with complement an external symbol
;; in the set package.
(defun complement-unlabeled-sets (s1 s2)
  (cond ((null s1) nil)
        ((null s2) s1)
        ((member (common-lisp:first s1) s2) (complement-unlabeled-sets (common-lisp:rest s1) s2))
        (t (cons (common-lisp:first s1) (complement-unlabeled-sets (common-lisp:rest s1) s2)))))

(shadow 'common-lisp:complement)
(defun complement (s1 s2)
  "Returns the relative complement of sets s1 and s2 (elements in s1 which are not also in s2)."
  (check-type s1 set)
  (check-type s2 set)
  (makeset (complement-unlabeled-sets (common-lisp:rest s1) (common-lisp:rest s2))))

;; 17.26 A set s1 is a subset of a set s2 if every element of s1 is a member of s2.
;; Define (subsetp s1 s2) in your set file to return True if the set s1 is a subset
;; of the set s2, and False otherwise.  Make subsetp an external symbol in the set
;; package, and shadow lisp:subsetp

(defun subsetp-unlabeled-sets (s1 s2)
  (cond ((null s1) t)
        ((null s2) nil)
        ((member (car s1) s2) (subsetp-unlabeled-sets (cdr s1) s2))
        (t nil)))

(shadow 'common-lisp:subsetp)
(defun subsetp (s1 s2)
  (check-type s1 set)
  (check-type s2 set)
  (if (subsetp-unlabeled-sets (common-lisp:rest s1) (common-lisp:rest s2)) t
      nil))

;; wow, that one was easy.


;; 17.27 Two sets are equal if they have exactly the same elements. Define
;; (equal s1 s2) in your set file to return True if s1 and s2 are equal sets,
;; and False otherwise.  Make sure that it does not matter if one set has its
;; elements in a different order than the other set, and make sure that the order
;; of the two arguments of set-equal is irrelevant.  Make this equal an external
;; symbol in the set package and shadow lisp:equal in that package

(defun unlabeled-equal (s1 s2)
  "Returns T if S1 and S2 are equal sets, regardless of order."
  (and (subsetp-unlabeled-sets s1 s2)
       (subsetp-unlabeled-sets s2 s1)))

(shadow 'common-lisp:equal)
(defun equal (s1 s2)
  (check-type s1 set)
  (check-type s2 set)
  (if (unlabeled-equal (common-lisp:rest s1) (common-lisp:rest s2)) t
      nil))

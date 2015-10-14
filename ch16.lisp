(defpackage :ch16
  (:use :common-lisp)
  (:shadow :length :member))

(in-package :ch16)

(defun length (l)
  "Returns the number of members in the argument list."
  (check-type l list)
  (if (null l) 0
      (1+ (length (rest l)))))

(defun member (obj l)
  "Returns True if OBJ is eql to a member of list L,
nil otherwise."
  (check-type l list)
  (cond ((null l) nil)
        ((eql obj (first l)) l)
        (t (member obj (rest l)))))

(defun number-listp (l)
  "Returns T if all members of the list L are numbers,
NIL otherwise."
  (assert (listp l) (l)
          "L must be a list, instead it is ~S." l)
  (cond ((null l) t)
        ((numberp (first l)) (number-listp (rest l)))
        (t nil)))

(defun same-length1 (l1 l2)
  "Returns T if the lists L1 and L2 have the same length,
NIL otherwise"
  (assert (listp l1) (l1)
          "L1 must be a list, instead it is ~S." l1)
  (assert (listp l2) (l2)
          "L2 must be a list, instead it is ~S." l2)
  (= (length l1) (length l2)))

(defun same-length2 (l1 l2)
  "Returns T if the lists L1 and L2 have the same length,
NIL otherwise"
  (assert (listp l1) (l1)
          "L1 must be a list, instead it is ~S." l1)
  (assert (listp l2) (l2)
          "L2 must be a list, instead it is ~S." l2)
  (cond ((null l1) (null l2))
        ((null l2) nil)
        (t (same-length2 (rest l1) (rest l2)))))

;; 16.6
(defun before (e1 e2 l)
  "If e1 appears before e2 in list l, return t"
  (check-type e1 (satisfies util:elementp))
  (check-type e2 (satisfies util:elementp))
  (check-type l list)
  (member e2 (member e1 l)))

;; 16.10 Define the function (count e l) to return the number of times
;; that the element e appears as a member of the list l.  You may have to
;; shadow lisp:count before redefining it.
;; (Hint: Two of three cond pairs will cause recursion)
(defun mycount (e l)
  "Return the number of times element E appears in list L"
  (check-type e (satisfies util:elementp))
  (check-type l list)
  (cond ((null l) 0)
        ((not (eql e (first l))) (mycount e (rest l)))
        (t (1+ (mycount e (rest l))))))


;; 16.11 Define the function (equal-lelt l1 l2), where l1 and l2 are lists of
;; elements (all members are elements) and equal-lelt returns T if the
;; corresponding members of l1 and l2 are eql, but nil if they are not.
;; Hint: in my version, only the third of four cond pairs causes recursion

;; conds:
;; lists are not same length: nil
;; first l1 eql first l2
;; first l1 not eql l2
;; lists are empty
(defun equal-lelt (l1 l2)
  (check-type l1 list)
  (check-type l2 list)
  (cond ((and (null l1) (null l2)) t)
        ((not (= (length l1) (length l2))) nil)
        ((eql (first l1) (first l2)) (equal-lelt (rest l1) (rest l2)))
         (t nil)))

;; 16.12 Shadow lisp:nth and then define the function (nth n l), where n
;; is an integer and l is a list, to return the nth member of l.
;; Compare ch16::nth with lisp:nth.

(defun my-nth (n l)
  "Return the nth member of list l"
  (check-type n integer)
  (check-type l list)
  (cond
        ((null l) nil)
        ((> n (length l)) nil)
        ((= (1- n) 0) (first l))
        (t (my-nth (1- n) (rest l)))))


;; 16.13 Define (allbut n l), where n is an integer and l is a list at least
;; n members long.  allbut should return a list whose members are the members
;; of l omitting the first n.  For example, (allbut 3 '(a b (c d) e f)) should
;; be (e f).  Common Lisp already has the function nthcdr which works just like
;; allbut.  Try nthcdr with several examples.


;; 16.14 Define the function (assoc e al) where e is an element and al is a
;; list all of whose members are lists. The function should return the first
;; element of al whose first member is eql to e.  For example:
;; (assoc 'Mary
;;     '((John black hair brown eyes)
;;      '(Mary blond hair blue eyes)
;;      '(Sue red hair hazel eyes)))
;; should return (Mary blond hair blue eyes).  We are treating al as an
;; association list in that we can associate a list of properties with each
;; element that is the first member of a member list of al.  Common Lisp
;; already has lisp:assoc defined, so shadow it. Use the CL version in the future
;; whenever you need its functionality.


;; 16.15 In your match.lisp file, define the function (matchelt l1 l2) to be like
;; equal-lelt except to consider the symbol ? (recognized by dont-care)
;; to be eql anything.  For example,
;; (matchlelt '(a ? c d e) '(a b c ? e)) should return t



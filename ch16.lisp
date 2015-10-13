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

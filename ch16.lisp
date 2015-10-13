(defpackage :ch16
  (:use :common-lisp)
  (:shadow :length :member))

(in-package :ch16)

(defun length (l)
  "Returns the number of members in the argument list."
  (assert (listp l) (l)
          "L must be a list, instead it is ~S." l)
  (if (null l) 0
      (1+ (length (rest l)))))

(defun member (obj l)
  "Returns True if OBJ is eql to a member of list L,
nil otherwise."
  (assert (listp l) (l)
          "L must be a list, instead it is ~S." l)
  (cond ((null l) nil)
        ((eql obj (first l)) t)
        (t (member obj (rest l)))))

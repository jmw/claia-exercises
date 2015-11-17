(defpackage :ch18
  (:use :common-lisp))
(in-package :ch18)

(defun tree-equal (t1 t2)
  "Returns T if T1 and T2 are trees with:
      1. the same structure,
      2. equal corresponding leaves (according to atom-equal);
   NIL otherwise."
  ;; T1 and T2 can be any objects.
  (cond ((atom t1) (atom-equal t1 t2))
        ((atom t2) nil)
        ((tree-equal (first t1) (first t2))
         (tree-equal (rest t1) (rest t2)))
        (t nil)))

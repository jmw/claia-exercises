(defpackage :util
  (:use :common-lisp))
(in-package :util)

(defun elementp (obj)
  "Return t if obj is a symbol, character, number, or package"
  (or (symbolp obj) (characterp obj) (numberp obj) (packagep obj)))

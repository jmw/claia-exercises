(defpackage :util
  (:use :common-lisp)
  (:export :elementp :element))

(in-package :util)

(defun elementp (obj)
  "Return t if obj is a symbol, character, number, or package"
  (or (symbolp obj) (characterp obj) (numberp obj) (packagep obj)))

(deftype element () "Elements are objects testable by EQL,
namely symbols, characters, numbers, and packages."
         '(satisfies util:elementp))


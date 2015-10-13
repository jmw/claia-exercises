(defpackage :calculator
  (:use common-lisp))

(in-package :calculator)

(defun combine-expr (operator operand expression)
  "Returns an expression with the operand applied by the operator to the first element of the expression"
  (cons (list operand operator (first expression)) (rest expression)))

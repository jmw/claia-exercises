(defpackage :calculator
  (:use common-lisp))

(in-package :calculator)

;; 17.33 Revising combine-expr to return first member in Cambridge Prefix notation
(defun combine-expr (operator operand expression)
  "Returns an expression with the operand applied by the operator to the first element of the expression"
;;  (cons (list operand operator (first expression)) (rest expression)))
  (cons (list operator operand (first expression)) (rest expression)))

;; 17.34 Write the function enclose-expression to take a list representing an arithmetic expression in normal infix notation and return a list whose one member is the expression transformed into Cambridge Prefix notation.  For now, assume that the only operators in the expression are + and -.  For example, (enclose-expression '(5 + 3 - 2)) sould evaluate to ((- (+ 5 3) 2)).  Hint: use your new version of combine-expr in the recursive step of your function.

;; 17.35 A term is one of the operands of addition or subtraction.  For example, in teh expression 5 - 4 + 3, the first term is 5, and in the expression 5*3/2+7-8, the first term is 5*3/2.  Define the function enclose-term to take a list like (5 - 4 + 3) or (5 * 3 / 2 + 7 - 8) and return it with the first term collected as the first member and expressed in Cambridge Prefix notation.  That is,
;; (enclose-term '(5 - 4 + 3))
;; should return
;; (5 - 4 + 3)
;; and
;; (enclose-term '(5 * 3 / 2 + 7 - 8))
;; should return
;; ((/ (* 5 3) 2) + 7 - 8)
;; for now, assume the only operators in the expression given to enclose-term
;; are +, -, *, and /.  Add enclose-term to your calculator.lisp file.

;; 17.36 A factor is one of the operands of multiplication or division. For
;; example, in the expression 5∗4+3, the first factor is 5, and in the expres-
;; sion 5^3^2 /7−8, the first factor is 5^3^2 . Define the function enclose-factor
;; to take a list like (5 * 4 + 3) or (5 ^ 3 ^ 2 / 7 - 8)
;; and return it with the first factor collected as the first member. That is,
;; (enclose-factor ’(5 * 4 + 3))
;; should return
;; (5 * 4 + 3)
;; and
;; (enclose-factor ’(5 ^ 3 ^ 2 / 7 - 8))
;; should return
;; ((^ 5 (^ 3 2)) / 7 - 8)
;; Add enclose-factor to your calculator file.


;(defun mydivcomp (x y)
;  (or (> (/ x y) 100) nil))

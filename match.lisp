(defpackage :match
  (:use :common-lisp))
(in-package :match)

(defun isqmark (n)
  (and (symbolp n) (string= (symbol-name n) "?")))

(defun variablep (s)
  "Return t if the first character of a symbol is '?'"
	 (char= #\? (char (symbol-name s) 0)))

(defun nullp (obj)
  "Return t if obj is nil, otherwise return nil"
  (eq nil obj))

(defun myconsp (obj)
  "Return t if obj is a nonempty list"
  (and (listp obj) (> (length obj) 0)))

(defun gt5 (obj)
  "Return t if obj is a string containing >5 characters or a list containing >5 members"
  (and (or (stringp obj) (listp obj)) (> (length obj) 5)))

;(defun match-element (e1 e2)
;  "return t if e1 and e2 are eql or either one is a variable as recognized by match::variablep"
;  (or (eql e1 e2) (variablep e1) (variablep e2)))

(defun match-element (e1 e2)
  "return t if e1 and e2 are eql or either one is ?;
   if either arg is a variable as recognized by match::variablep,
   return a 2-element list whose first member is the variable and the second
   is the other argument.  Otherwise, return nil."
  (cond ((or (eql e1 e2) (isqmark e1) (isqmark e2)) t)
        ((variablep e1) (list e1 e2))
        ((variablep e2) (list e2 e1))
        (t nil)))

;(defun absval (n)
;  (if (< n 0) (- n) n))

(defun absval (n)
  (cond ((< n 0) (- n))
        (t n)))

(defun sign (n)
  "Takes a numeric argument and returns
   - if it is negative,
   0 if it is zero, and
   + if it is positive"
  (cond ((< n 0) '-)
        ((= n 0) 0)
        ((> n 0) '+)))

(defun dont-care (n)
  "Return t if the argument is a question mark symbol, otherwise nil"
  (and (symbolp n) (string= (symbol-name n) "?")))

(defun sum (n1 n2)
  "Returns the sum of two nonnegative integers."
  (assert
   (and (integerp n1) (>= n1 0))
   (n1)
   "n1 must be a nonnegative integer; instead it's ~S."
   n1)
  (assert
   (integerp n2)
   (n2)
   "n2 must be an integer; instead it's ~S."
   n2)
  (if (zerop n1) n2
      (sum (1- n1) (1+ n2))))

(defun product (n1 n2)
  "Multiplies 2 nonnegative integers using only zerop, 1-, sum"
  (assert
   (and (integerp n1) (>= n1 0))
   (n1)
   "n1 must be a nonnegative integer, but it's ~S."
   n1)
  (assert (integerp n2)
          (n2)
          "n2 must be an integer, but it's ~S."
          n2)
  (cond ((zerop n1) 0)
        (t (sum n2 (product (1- n1) n2)))))

(defun power (n i)
  "Takes 2 nonnegative integers, n and i.
   Returns n to the i power, using only zerop, 1-, product"
  (assert
   (and (integerp n) (>= n 0))
        (n)
        "n must be a nonnegative integer, but it's ~S."
        n)
  (assert
   (and (integerp i) (>= i 0))
          (i)
          "i must be a nonnegative integer, but it's ~S."
          i)
  (cond ((zerop i) 1)
        (t (product n (power n (1- i))))))

;;; Shadow any symbols from automatically inherited
;;; packages that have the same names as symbols
;;; in this package
;(shadow '(symbol1, symbol2, ...))

;;; Shadow and import any symbols from other packages
;;; that are to be available as internal symbols
;;; in this package,  but whose names conflict with
;;; symbols from automatically inherited packages
;(shadowing-import '(symbol1, symbol2...))

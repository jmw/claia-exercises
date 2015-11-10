(defpackage :match
  (:use :common-lisp)
  (:export :variablep :variable))

(in-package :match)

(defun isqmark (n)
  (and (symbolp n) (string= (symbol-name n) "?")))

(defun variablep (s)
  "Return t if the first character of a symbol is '?'"
	 (char= #\? (char (symbol-name s) 0)))

(shadow 'common-lisp:variable)
(deftype variable ()
  "A variable is a symbol which begins with a '?'"
  '(satisfies match:variablep))

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

;; 16.15 In your match.lisp file, define the function (matchelt l1 l2) to be like
;; equal-lelt except to consider the symbol ? (recognized by dont-care)
;; to be eql anything.  For example,
;; (matchlelt '(a ? c d e) '(a b c ? e)) should return t
(defun match-lelt (l1 l2)
  (check-type l1 list)
  (check-type l2 list)
  (cond ((and (null l1) (null l2)) t)
        ((not (= (length l1) (length l2))) nil)
        ((or (or (dont-care (first l1)) (dont-care (first l2)))
             (eql (first l1) (first l2))) (match-lelt (rest l1) (rest l2)))
        (t nil)))


;; 17.29 ... Add (boundp v subs) that returns True if the variable v
;; (as recognized by variablep) is bound to anything in the substitution subs
;; and False otherwise.  You may use the Lisp function assoc in this definition.
;; You will have to shadow common-lisp:boundp in your match file.
(shadow 'common-lisp:boundp)
(defun boundp (v subs)
  (check-type v variable)
  (if (assoc v subs) t
      nil))

;; 17.30 Add to your match file a function (bound-to v subs) that returns
;; the term that the variable v is bound to in the substitution subs, or nil
;; if v is unbound in subs
;; For example:
;;   (bound-to '?c '((?a . 1) (?b . 2) (?c . 3)))
;; returns:
;;   3
(defun bound-to (v subs)
  (check-type v variable)
  (if (assoc v subs) (cdr (assoc v subs))
      nil))

;; 17.31 Add too your match file a function (match pat lst), where pat and lst
;; are both lists of elements.  match should return a substitution--a list of all
;; pairs (v a) where v is a variable in pat and a is the corresponding element in list.
;; If the nth member of pat is not a variable, it must be eql to the nth member of lst.
;; Otherwise match should return nil.  If no element of pat is a variable but each is
;; eql to its corresponding element of lst, match should return ((T T)).  If a
;; variable occurs more than once in pat, its corresponding elements in lst must be
;; the same.  For example:
;;   > (match '(a b c) '(a b c))
;;     ((T T))
;;   > (match '(a b c) '(a c b))
;;     NIL
;;   > (match '(a ?x c) '(a b c))
;;     ((?X B) (T T))
;;   > (match '(a ?x c ?x) '(a b c d))
;;     NIL
;;   > (match '(a ?x c ?x) '(a b c b))
;;     ((?X B) (T T))
;;   > (match '(a ?x c ?y) '(a b c d))
;;     ((?Y D) (?X B) (T T))
;; The order of pairs in your answer needn't be the same as above.
;; Hint: You may find it useful to define a help function
;; (match1 pat lst pairs)


;; 17.32 In your match file, define (substitute pat subs), where pat is a list
;; liike the first argument of match, subs is a substitution, and substitute
;; returns a list like pat except every variable in pat that is bound in subs
;; is replaced by the element it is bound to.  For every appropriate pat and lst,
;; it should be the case that (substitute pat (match pat lst)) = lst.  Shadow
;; common-lisp:substitute in your match file.



;;; Shadow any symbols from automatically inherited
;;; packages that have the same names as symbols
;;; in this package
;(shadow '(symbol1, symbol2, ...))

;;; Shadow and import any symbols from other packages
;;; that are to be available as internal symbols
;;; in this package,  but whose names conflict with
;;; symbols from automatically inherited packages
;(shadowing-import '(symbol1, symbol2...))

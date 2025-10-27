;;; bindings.el --- define standard key bindings and some variables  -*- lexical-binding: t; -*-
;;
;; This is a stub for bootstrapping rune

(defmacro bound-and-true-p (var)
  "Return the value of symbol VAR if it is bound, else nil.
Note that if `lexical-binding' is in effect, this function isn't
meaningful if it refers to a lexically bound variable."
  `(and (boundp (quote ,var)) ,var))

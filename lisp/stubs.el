;;; -*- lexical-binding: t; -*-


(defun help-uni-confusable-suggestions (string)
  "stub of function for bootstrapping"
  nil)

(defun help-add-fundoc-usage (docstring arglist)
  "stub of function for bootstrapping"
  docstring)

(defun substitute-command-keys (string &optional no-face)
  "stub of function for bootstrapping"
  string)

(defun help--docstring-quote (string)
  "stub of function for bootstrapping"
  string)

(defun help-add-fundoc-usage (docstring arglist)
  "stub of function for bootstrapping"
  docstring)

(defun help-split-fundoc (docstring def &optional section)
  "stub of function for bootstrapping"
  (cons docstring ""))


(defmacro trace (type &rest body)
  `(prog2 (message "BEGIN: %s" ,type)
       ,@body
     (message "END: %s" ,type)))

(defmacro trace-return (type &rest body)
  `(progn (message "BEGIN: %s" ,type)
       (let ((val ,@body))
         (message "RETURN: %s: %s" ,type val)
         val)))

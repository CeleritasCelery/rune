;;; -*- lexical-binding: t; -*-

(let ((print-gensym t)
      (print-circle t))
  (print (macroexpand '(push (car a) (cdr b))))
  nil)

(print (macroexpand '(push (car a) (cdr b))))

(setq foo
      (let ((a '(x))
            (b '((x))))
        (push (car a) (cdr b))
        b))

(message "printing")

(message "foo: %s" foo)

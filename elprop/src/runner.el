;;; -*- lexical-binding: t; -*-

(defvar eprop-output-buffer (get-buffer-create "eprop-out"))

(defmacro eprop-runner (id body)
  `(let (result)
    (setq result
          (condition-case err
               ,body
             (error err)))
    (with-current-buffer eprop-output-buffer
      (insert (format ";; %s\n%s\n\n" ,id result)))))

;;; -*- lexical-binding: t; -*-

(defvar runner-fail nil)
(defvar runner-count 0)

(defun elprop-process-filter (proc output)
  "Filter function for the runner process.
   Accumulates output until a complete s-expression is received,
   then evaluates it and sends the result back to the process."
  (condition-case _
      (with-current-buffer (process-buffer proc)
        (save-excursion
          (goto-char (point-max))
          (insert output)
          (goto-char (point-min))
          (when (search-forward "thread 'main' panicked" nil t)
            (setq runner-fail t)))
        (when (re-search-forward "^proptest: " nil t)
          (forward-line))
        (while-let ((sexp (and (re-search-forward ";; ELPROP_START" nil t)
                               (condition-case e
                                   (read (current-buffer))
                                 (end-of-file nil)))))
          (process-send-string proc (format ";; ELPROP_START:%d\n%S\n;; ELPROP_END\n"
                                            runner-count
                                            (condition-case err
                                                (eval sexp)
                                              (error err))))
          (cl-incf runner-count)))
    (error (setq runner-fail t))))

(when (null noninteractive)
  (setq runner-fail nil
        runner-count 0)
  (kill-buffer "*elprop-output*"))

(let* ((buffer "*elprop-output*")
       (process (start-process "elprop-process" buffer (getenv "ELPROP_RUNNER"))))
  (set-process-filter process 'elprop-process-filter)
  (while (and (null runner-fail)
              (process-live-p process))
    (accept-process-output process)
    (sleep-for 0.05))
  (if runner-fail
      (error (format "Test Runner panicked\n%s"
                     (with-current-buffer buffer
                       (buffer-substring-no-properties (point-min) (point-max))))))
  (message "done"))

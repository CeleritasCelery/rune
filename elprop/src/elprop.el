;;; -*- lexical-binding: t; -*-

;; For interactive use from within Emacs.
;; This will reset the code to it's initial state.
;; make sure to set ELPROP_RUNNER as well
(setq text-quoting-style 'straight)

(when (null noninteractive)
  (setenv "ELPROP_RUNNER" "~/rune/target/debug/runner")
  (kill-buffer "*elprop-output*"))

(let* ((buffer "*elprop-output*")
       (process (start-process "elprop-process" buffer (getenv "ELPROP_RUNNER")))
       (runner-count 0)
       (pointer 1))
  (with-current-buffer buffer
    (while (process-live-p process)
      (save-excursion
        (goto-char (point-min))
        (when (search-forward "thread 'main' panicked" nil t)
          (search-backward ";; ELPROP_START" nil t)
          (let* ((backtrace (buffer-substring-no-properties
                             (point)
                             (point-max)))
                 (escaped (replace-regexp-in-string "%" "%%" backtrace)))
            (error escaped))))
      (goto-char pointer)
      (while-let ((sexp (and (search-forward ";; ELPROP_START" nil t)
                             (save-excursion (search-forward ";; ELPROP_END" nil t))
                             (condition-case e
                                 (read (current-buffer))
                               (end-of-file nil)))))
        (setq pointer (point))
        (let ((result (condition-case err
                          (eval sexp)
                        (error err))))
          (process-send-string process (format ";; ELPROP_START:%d\n%S\n;; ELPROP_END\n"
                                               runner-count
                                               result)))
        (cl-incf runner-count))
      (sleep-for 0.01)))
  (message "done"))

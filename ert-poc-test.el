;;; ert-poc-test.el -*- lexical-binding: t; -*-

(require 'cl-extra)

(load "test/src/fns-tests.el")

(defvar inhibit-redisplay t)
(defvar message-log-max 1000)
(defvar fns-tests)
(defvar alltestsselector)

(put 'wrong-type-argument 'error-conditions (list 'wrong-type-argument 'error))

(defun messages-buffer () (get-buffer-create "*Messages*"))

(defun point-max-marker () (length (buffer-substring 0 0)))

(defun set-marker (MARKER POSITION &optional BUFFER))

(setq fns-tests '(fns-tests-identity
                  ;; fns-tests-random
                  fns-tests-length
                  fns-tests-safe-length
                  fns-tests-string-bytes
                  ;; fns-tests-equality-nan
                  ;; fns-tests-reverse
                  ;; fns-tests-nreverse
                  ;; fns-tests-reverse-bool-vector
                  ;; fns-tests-nreverse-bool-vector
                  ;; fns-tests-string-lessp
                  ;; fns-tests-compare-strings
                  ;; fns-tests-collate-strings
                  ;; fns-tests-sort
                  ;; fns-tests-collate-sort
                  ;; fns-tests-string-version-lessp
                  ;; fns-tests-func-arity
                  ;; fns-tests-base64-encode-region
                  ;; fns-tests-base64-encode-string
                  ;; fns-test-base64url-encode-region
                  ;; fns-test-base64url-encode-string
                  ;; fns-tests-base64-decode-string
                  ;; fns-tests-hash-buffer
                  ;; fns-tests-mapconcat
                  ;; fns-tests-mapcan
                  ;; test-cycle-length
                  ;; test-cycle-safe-length
                  ;; test-cycle-member
                  ;; test-cycle-memq
                  ;; test-cycle-memql
                  ;; test-cycle-assq
                  ;; test-cycle-assoc
                  ;; test-assoc-testfn
                  ;; test-cycle-rassq
                  ;; test-cycle-rassoc
                  ;; test-cycle-delq
                  ;; test-cycle-delete
                  ;; test-cycle-reverse
                  ;; test-cycle-equal
                  ;; test-cycle-nconc
                  ;; test-cycle-plist-get
                  ;; test-cycle-plist-member
                  ;; test-cycle-plist-put
                  ;; plist-get/odd-number-of-elements
                  ;; plist-put/odd-number-of-elements
                  ;; plist-member/improper-list
                  ;; test-plist
                  ;; test-string-distance
                  ;; test-bignum-eql
                  ;; test-bignum-hash
                  ;; test-nthcdr-simple
                  ;; test-nthcdr-circular
                  ;; test-proper-list-p
                  ;; test-hash-function-that-mutates-hash-table
                  ;; test-sxhash-equal
                  ;; test-secure-hash
                  ;; test-vector-delete
                  ;; string-search
                  ;; object-intervals
                  ;; length-equals-tests
                  ;; test-buffer-line-stats-nogap
                  ;; test-buffer-line-stats-gap
                  ;; test-line-number-at-position
                  ;; fns-vconcat
                  ;; fns-append
                  ;; fns--string-to-unibyte-multibyte
                  ;; fns--take-ntake
                  ;; fns--copy-alist
                  )
      alltestsselector (cons 'member fns-tests))

(progn (ert-run-tests-batch alltestsselector) 1)

;;; run-test.el

;;; Description:
;;  emacs -batch -Q -l test/run-test.el

(defvar namelist-test-dir (file-name-directory (or load-file-name
                                              buffer-file-name)))

(defvar namelist-root-dir (file-truename
                      (concat namelist-test-dir "../")))

(setq debug-on-error t)
;;; load-path
(mapc (lambda (p) (add-to-list 'load-path p))
      (list namelist-test-dir namelist-root-dir))

;; Use ERT from github when this Emacs does not have it
(unless (locate-library "ert")
  (add-to-list
   'load-path
   (file-name-as-directory (concat namelist-root-dir "/lib/ert/lisp/emacs-lisp")))
  (require 'ert-batch)
  (require 'ert-ui))

;; load tests
(load "namelist-mode-tests")

;;; run
(if noninteractive
    (progn
      (princ emacs-version)
      (ert-run-tests-batch-and-exit))
  (ert t))

;;; run-test.el ends here

;;; namelist-mode-tests.el
;;
;; Author: Yagnesh Raghava Yakkala. http://yagnesh.org
;;    File: namelist-mode-tests.el
;; Created: Thursday, February 21 2013

;;; Description:
;;
(require 'namelist-mode)

(defun namelist-should-indent (content column)
  "Assert indentation COLUMN on the last line of CONTENT."
  (with-temp-buffer
    (insert content)
    (namelist-mode)
    (namelist-indent-line)
    (should (= (current-indentation) column))))

(defun namelist-should-indent-buffer (expected content)
  "Assert that CONTENT turns into EXPECTED after the buffer is re-indented.

The whitespace before and including \"|\" on each line is removed."
  (with-temp-buffer
    (cl-flet ((fix-indent (s) (replace-regexp-in-string "^[ \t]*|" "" s)))
      (insert (fix-indent content))
      (namelist-mode)
      (indent-region (point-min) (point-max))
      (should (string= (fix-indent expected) (buffer-string))))))


;;; indentation
(ert-deftest namelist-test-group-begin ()
  (namelist-should-indent "   &group" 0)
  (namelist-should-indent "    &group\n  /" 0))

(ert-deftest namelist-test-group-end ()
  (namelist-should-indent "   /" 0)
  (namelist-should-indent "     /" 0))

(ert-deftest namelist-indent-black ()
  (namelist-should-indent-buffer "  &group\nfoo = bar\n \n  /"
                                 "&group\n  foo = bar\n/"))


;;; namelist-mode-tests.el ends here

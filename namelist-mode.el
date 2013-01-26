;;; namelist-mode.el -- Major Mode for editing f90 namelist files.

;; Copyright (C) 2013 Yagnesh Raghava Yakkala <http://yagnesh.org>

;; Author: Yagnesh Raghava Yakkala <hi@yagnesh.org>
;; URL: https://github.com/yyr/namelist-mode
;; Maintainer: Yagnesh Raghava Yakkala <hi@yagnesh.org>
;; Version: 0.1-dev
;; Created: Thursday, January 24 2013
;; Keywords: namelist, Major Mode, namelist-mode, Fortran

;; This file is NOT part of GNU Emacs.

;; namelist-mode.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; namelist-mode.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Major mode to edit F90 namelist files.

;;;###autoload
(defgroup namelist nil
  "Major mode to edit Fortran namelist files."
  :group 'languages)

(defcustom namelist-mode-hook nil
  "Hook that runs right after enabling namelist-mode."
  :type 'hook
  :group 'namelist)

(defcustom namelist-imenu-generic-expression
  '(("Groups" "^[[:blank:]]*&\\(.*\\)$" 1))
  "Generic expression for matching group title in the namelist."
  :type 'string
  :group 'namelist)

;;; indention.

;;; navigation.


;;; font lock
(defvar namelist-group-begin-re "^[[:blank:]]*&\\(.*\)) *$"
  "regex to match beginning of namelist Group.")

(defvar namelist-group-end-re
  "\\(?:&end\\|/\\)"
  "regex to end  of namelist Group.")

(defvar namelist-keys-re
  (concat
   "^[ \t]*"                            ;initial optional space
   "\\([a-zA-Z0-9_]*\\).*="                   ; keys
   )
  "Regexp for matching variable.")

(defconst namelist-font-lock-keywords
  (list
   (cons namelist-group-begin-re '(1 font-lock-builtin-face))
   (cons namelist-group-end-re '(1 font-lock-builtin-face))
   (cons namelist-keys-re '(1 font-lock-keyword-face)))

  "Font lock keyword regular expressions for namelist mode.")

;;; define mode
(defvar namelist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-m") 'reindent-then-newline-and-indent)
    map)

  "Key map for namelist mode.")

;;;###autoload
(define-derived-mode namelist-mode prog-mode "namelist"
  "Major mode for editing f90 namelist files.

\\{namelist-mode-map}"

  (set (make-local-variable 'comment-start) "!")
  (setq indent-tabs-mode nil)
  (set (make-local-variable 'imenu-generic-expression)
       namelist-imenu-generic-expression)
  (set (make-local-variable 'font-lock-defaults)
       'namelist-font-lock-keywords))


;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.namelist" . 'namelist-mode))

;;; namelist-mode.el ends here

;;; namelist-mode.el -- Major Mode for editing f90 namelist files.

;; Copyright (C) 2013 Yagnesh Raghava Yakkala <http://yagnesh.org>

;; Author: Yagnesh Raghava Yakkala <hi@yagnesh.org>
;; URL: https://github.com/yyr/ncl-mode
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


;;; define mode
(defvar namelist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-m") 'reindent-then-newline-and-indent)

    "Key map for ncl mode."))

;;;###autoload
(define-derived-mode namelist-mode prog-mode "namelist"
  "Major mode for editing f90 namelist files.

\\{namelist-mode-map}"

  (set (make-local-variable 'comment-start) "!")
  (setq indent-tabs-mode nil)
  (set (make-local-variable 'imenu-generic-expression)
       namelist-imenu-generic-expression)
  )

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.namelist" . 'namelist-mode))

;;; namelist-mode.el ends here

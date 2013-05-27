;;; namelist-mode-load.el --- loading script for namelist-mode (and other things)

;; Copyright (C) 2012, 2013 Yagnesh Raghava Yakkala <http://yagnesh.org>

;; Author: Yagnesh Raghava Yakkala <hi@yagnesh.org>
;; URL: https://github.com/yyr/namelist-mode
;; Maintainer: Yagnesh Raghava Yakkala <hi@yagnesh.org>
;; Created: Sunday, may 28 2013
;; Version: 0.2
;; Keywords: run, namelist, inferior, shell

;; This file is NOT part of GNU Emacs.

;; namelist-mode-load.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; namelist-doc.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; User initialization file.
;; place following line in your .emacs file
;; (load "path/to/this/file/emacs-namelist-load.el")

;;; Code:

;; find this file path and it to load-path
(defconst namelist-mode-dir (file-name-directory
                        (or load-file-name buffer-file-name)))

(add-to-list 'load-path (file-truename
                         (concat namelist-mode-dir "lisp")))


;;; General autoload namelist mode
;;=================================================================
(add-to-list 'auto-mode-alist (cons (purecopy "\\.namelist\\'") 'namelist-mode))
(autoload 'namelist-mode "namelist-mode.el" "namelist-mode for editing ncar graphics" t)

;; Yasnippet setup
;;=================================================================
(when (require 'yasnippet nil t)
  (add-to-list 'yas/snippet-dirs (concat namelist-mode-dir "snippets/")))

(provide 'namelist-mode-load)
;;; namelist-mode-load.el ends here

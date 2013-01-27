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
;;
;; Example namelist file looks like the following.
;;______________________________________
;; &group1
;;   key1 = value1, value2
;;   key2 = value1
;;   ...
;; $end
;; &group2
;;   ...
;; $end
;;______________________________________

;;; Code:
;;;###autoload
(defgroup namelist nil
  "Major mode to edit Fortran namelist files."
  :group 'languages)

(defcustom namelist-mode-hook nil
  "Hook that runs right after enabling `namelist-mode'."
  :type 'hook
  :group 'namelist)

(defcustom namelist-imenu-generic-expression
  '(("Groups" "^[[:blank:]]*&\\(.*\\)$" 1))
  "Generic expression for matching group title in the namelist."
  :type 'string
  :group 'namelist)

;;; Indention.
(defcustom namelist-indent-offset 2
  "Namelist indentation of a block."
  :type 'integer
  :group 'namelist)

(defcustom namelist-comment-char "!"
  "Comment character. Its generally `!'. But occasionally
different character is used."
  :type 'character
  :group 'namelist)

(defsubst namelist-point-in-group-p ()
  "Check and return `t' if the point is in a group i.e, in between
&group ... $end."
  (let ((ep (save-excursion
              (re-search-backward namelist-group-end-re nil t)))
        (bp (save-excursion
              (re-search-backward namelist-group-begin-re nil t))))
    (cond ((and ep bp
                (> ep bp))
           nil)
          ((and ep bp
                (< ep bp))
           t)
          (bp
           (if bp t nil)))))

(defun namelist-indent-comment ()
  "Returns indentation level for comments. If comment is inside a
  group return value is `namelist-indent-offset'. If the comment
  is outside of any group return value is 0."
  (if (namelist-point-in-group-p)
      namelist-indent-offset
    0))

(defun namelist-calculate-indent ()
  "Calculate indent for current column."
  (save-excursion
    (back-to-indentation)
    (cond ((or (looking-at namelist-group-end-re)
               (looking-at namelist-group-end-re))
           0)
          ((namelist-point-in-group-p)
           namelist-indent-offset)
          (t
           0))))


(defun namelist-indent-line ()
  "Indent current line."
  (interactive "*")
  (let ((pos (point-marker))
        indent)
    (back-to-indentation)
    (if (looking-at namelist-comment-char)
        (setq indent (namelist-comment-indent))
      (setq indent (namelist-calculate-indent)))
    (or (= indent (current-column))
        (progn
          (back-to-indentation)
          (delete-horizontal-space)
          (indent-to indent)))
    (and (< (point) pos)
         (goto-char pos))))

;;; Navigation.


;;; Font lock.
(defcustom namelist-builtin-keywords
  '(".true." ".false.")
  "Namelist allowed inbuilt keywords."
  :group 'namelist)

(defvar namelist-group-begin-re "^[[:blank:]]*&\\(.*\\) *$"
  "Regex to match beginning of namelist Group.")

(defvar namelist-group-end-re
  (regexp-opt '("$end" "$END" "/") 'paren)
  "Regex to end  of namelist Group.")

(defvar namelist-key-re                 ; key = value
  (concat
   "^[ \t]*"
   "\\([a-zA-Z0-9_]*\\)"                ; var
   "[ \t]*="                            ; space=
   ;; \\[:blank:\\]
   ;; "[\\(.*\\),]+"
   )
  "Regexp for matching variable.")

(defconst namelist-font-lock-keywords
  (list
   (cons (regexp-opt namelist-builtin-keywords 'paren) '(1 font-lock-builtin-face))
   (cons namelist-group-begin-re '(1 font-lock-builtin-face))
   (cons namelist-key-re '(1 font-lock-keyword-face))
   (cons namelist-group-end-re '(1 font-lock-builtin-face))
   )
  "Font lock keyword regular expressions for namelist mode.")

;;; Major mode.
(defvar namelist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-m") 'reindent-then-newline-and-indent)
    map)

  "Key map for namelist mode.")

(defvar namelist-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\r " "  table) ; return is white space
    (modify-syntax-entry ?_  "w"  table) ; underscore in names
    (modify-syntax-entry ?\\ "\\" table) ; escape chars

    (modify-syntax-entry ?! "<"  table) ; begin comment
    (modify-syntax-entry ?\n ">"  table) ; end comment
    (modify-syntax-entry ?\f ">"  table)

    (modify-syntax-entry ?$  "_"  table) ; symbol constituents
    (modify-syntax-entry ?\` "_"  table)

    (modify-syntax-entry ?\' "\"" table) ; string quote
    (modify-syntax-entry ?\" "\"" table)

    (modify-syntax-entry ?.   "." table) ; punctuation
    (modify-syntax-entry ?<   "." table)
    (modify-syntax-entry ?>   "." table)
    (modify-syntax-entry ?+  "."  table)
    (modify-syntax-entry ?-  "."  table)
    (modify-syntax-entry ?=  "."  table)
    (modify-syntax-entry ?*  "."  table)
    (modify-syntax-entry ?/  "."  table)
    (modify-syntax-entry ?%  "."  table)
    (modify-syntax-entry ?#  "."  table)
    table)
  "Syntax table used in namelist mode.")

;;;###autoload
(define-derived-mode namelist-mode prog-mode "namelist"
  "Major mode for editing f90 namelist files.

\\{namelist-mode-map}"
  (set (make-local-variable 'indent-line-function) 'namelist-indent-line)
  (set (make-local-variable 'comment-start) namelist-comment-char)
  (setq indent-tabs-mode nil)
  (set (make-local-variable 'imenu-generic-expression)
       namelist-imenu-generic-expression)
  (set (make-local-variable 'font-lock-defaults)
       '(namelist-font-lock-keywords)))


;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.namelist" . 'namelist-mode))

;;; namelist-mode.el ends here

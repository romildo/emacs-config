;;; clean-mode.el --- A Concurrent Clean editing mode    -*-coding: iso-8859-1;-*-

;; Copyright (C) 2003, 2004  Free Software Foundation, Inc
;; Copyright (C) 1992, 1997-1998 Simon Marlow, Graeme E Moss, and Tommy Thorn

;; Authors: 1992      Simon Marlow
;;          1997-1998 Graeme E Moss <gem@cs.york.ac.uk> and
;;                    Tommy Thorn <thorn@irisa.fr>,
;;          2001-2002 Reuben Thomas (>=v1.4)
;;          2003      Dave Love <fx@gnu.org>
;;          2005      lethevert
;; Keywords: Concurrent Clean
;; Version: 0.1
;; URL: http://www.geocities.jp/lethevert/softwares/clean/index.html

;;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:

;; Purpose:
;;
;; To provide a pleasant mode to browse and edit Clean files, linking
;; into the following supported modules:
;;
;; `clean-font-lock', Graeme E Moss and Tommy Thorn and lethevert
;;   Fontifies standard Clean keywords, symbols, functions, etc.
;;
;; `clean-decl-scan', Graeme E Moss and lethevert
;;   Scans top-level declarations, and places them in a menu.
;;
;; `clean-doc', Hans-Wolfgang Loidl and lethevert
;;   Echoes types of functions or syntax of keywords when the cursor is idle.
;;
;; `clean-indent', Guy Lapalme and lethevert
;;   Intelligent semi-automatic indentation.
;;
;; `clean-simple-indent', Graeme E Moss and Heribert Schuetz and lethevert
;;   Simple indentation.
;;
;;
;; Installation:
;; 
;; Put in your ~/.emacs:
;;
;;    (setq auto-mode-alist
;;          (append auto-mode-alist
;;                  '(("\\.icl$"  . clean-mode)
;;                    ("\\.dcl$"  . clean-mode)
;;                    ("\\.prj$"  . clean-project-mode))))
;;
;;    (autoload 'clean-mode "clean-mode"
;;       "Major mode for editing Clean scripts." t)
;;    (autoload 'clean-project-mode "clean-project-mode"
;;       "Major mode for editing Clean Project Scripts." t)
;;
;; with `clean-mode.el' accessible somewhere on the load-path.
;; To add a directory `~/lib/emacs' (for example) to the load-path,
;; add the following to .emacs:
;;
;;    (setq load-path (cons "~/lib/emacs" load-path))
;;
;; To turn any of the supported modules on for all buffers, add the
;; appropriate line(s) to .emacs:
;;
;;    (add-hook 'clean-mode-hook 'turn-on-clean-decl-scan)
;;    (add-hook 'clean-mode-hook 'turn-on-clean-doc-mode)
;;    (add-hook 'clean-mode-hook 'turn-on-clean-indent)
;;    (add-hook 'clean-mode-hook 'turn-on-clean-simple-indent)
;;
;; Make sure the module files are also on the load-path.  Note that
;; the two indentation modules are mutually exclusive: Use only one.
;;
;;
;; Customisation:
;;
;; Also see the customisations of the modules.
;;
;;
;; History:
;;
;; This mode is created by lethevert 2/7/2005 based on haskell-mode 2.0
;; which is based on an editing mode by Simon Marlow 11/1/92 and
;; heavily modified by Graeme E Moss and Tommy Thorn 7/11/98.
;; 
;; If you have any problems or suggestions specific to a supported
;; module, consult that module for a list of known bugs, and an
;; author to contact via email.  For general problems or suggestions,
;; consult the list below, then email gem@cs.york.ac.uk and
;; thorn@irisa.fr quoting the version of the mode you are using, the
;; version of Emacs you are using, and a small example of the problem
;; or suggestion.
;;
;; Version 0.1
;;   convert from haskell-mode 2.0
;;   lethevert <lethevert@users.sourceforge.net>
;;

;;; Code:

(eval-when-compile (require 'cl))

;; Version of mode.
(defconst clean-version "0.1"
  "`clean-mode' version number.")
(defun clean-version ()
  "Echo the current version of `clean-mode' in the minibuffer."
  (interactive)
  (message "Using clean-mode version %s" clean-version))

(defgroup clean nil
  "Major mode for editing clean programs."
  :group 'languages
  :prefix "clean-")

;; Set up autoloads for the modules we supply
(autoload 'turn-on-clean-decl-scan "clean-decl-scan"
  "Turn on Clean declaration scanning." t)
(autoload 'turn-on-clean-doc-mode "clean-doc"
  "Turn on Clean Doc minor mode." t)
(autoload 'turn-on-clean-indent "clean-indent"
  "Turn on Clean indentation." t)
(autoload 'turn-on-clean-simple-indent "clean-simple-indent"
  "Turn on simple Clean indentation." t)

;; Functionality provided in other files.
(autoload 'clean-ds-create-imenu-index "clean-decl-scan")
(autoload 'clean-font-lock-choose-keywords "clean-font-lock")
(autoload 'clean-doc-current-info "clean-doc")

;; Mode maps.
(defvar clean-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Bindings for the inferior clean process:
    ;; (define-key map [?\M-C-x]     'inferior-clean-send-defun)
    ;; (define-key map [?\C-x ?\C-e] 'inferior-clean-send-last-sexp)
    ;; (define-key map [?\C-c ?\C-r] 'inferior-clean-send-region)
    (define-key map [?\C-c ?\C-z] 'switch-to-clean)
    (define-key map [?\C-c ?\C-l] 'inferior-clean-load-file)
    ;; Non standard in other inferior-modes, but traditional in clean.
    (define-key map [?\C-c ?\C-r] 'inferior-clean-reload-file)
    (define-key map [?\C-c ?\C-b] 'switch-to-clean)
    ;; (define-key map [?\C-c ?\C-s] 'inferior-clean-start-process)

    ;; That's what M-; is for.
    ;; (define-key map "\C-c\C-c" 'comment-region)
    map)
  "Keymap used in Clean mode.")

;; Syntax table.
(defvar clean-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\  " " table)
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\'" table)
    (modify-syntax-entry ?_  "w" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[  "(]" table)
    (modify-syntax-entry ?\]  ")[" table)
    (modify-syntax-entry ?\{  "(}" table)
    (modify-syntax-entry ?\}  "){" table)

    ;(cond ((featurep 'xemacs)
           ;; I don't know whether this is equivalent to the below
           ;; (modulo nesting).  -- fx
           ;(modify-syntax-entry ?{  "(}5" table)
           ;(modify-syntax-entry ?}  "){8" table)
           ;(modify-syntax-entry ?-  "_ 1267" table))
          ;(t
           ;; The following get comment syntax right, similarly to C++
           ;; In Emacs 21, the `n' indicates that they nest.
           ;; The `b' annotation is actually ignored because it's only
           ;; meaningful on the second char of a comment-starter, so
           ;; on Emacs 20 and before we get wrong results.  --Stef
           (modify-syntax-entry ?/  "_ 124" table)
           (modify-syntax-entry ?*  "_ 23" table)
           ;))
    (modify-syntax-entry ?\n ">" table)

    (let (i lim)
      (map-char-table
       (lambda (k v)
         (when (equal v '(1))
           ;; The current Emacs 22 codebase can pass either a char
           ;; or a char range.
           (if (consp k)
               (setq i (car k)
                     lim (cdr k))
             (setq i k 
                   lim k))
           (while (<= i lim)
             (when (> i 127)
               (modify-syntax-entry i "_" table))
             (setq i (1+ i)))))
       (standard-syntax-table)))

    (modify-syntax-entry ?\` "$`" table)
    (modify-syntax-entry ?\\ "\\" table)
    (mapcar (lambda (x)
              (modify-syntax-entry x "_" table))
            ;; Some of these are actually OK by default.
            "!#$%&+.:<=>?@^|~") ;"!#$%&*+./:<=>?@^|~")
    (unless (featurep 'mule)
      ;; Non-ASCII syntax should be OK, at least in Emacs.
      (mapcar (lambda (x)
                (modify-syntax-entry x "_" table))
              (concat "¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿"
                      "×÷"))
      (mapcar (lambda (x)
                (modify-syntax-entry x "w" table))
              (concat "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ"
                      "ØÙÚÛÜÝÞß"
                      "àáâãäåæçèéêëìíîïðñòóôõö"
                      "øùúûüýþÿ")))
    table)
  "Syntax table used in Clean mode.")

;; Various mode variables.
(defun clean-vars ()
  (kill-all-local-variables)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'comment-start)
  (setq comment-start "/*")
  (make-local-variable 'comment-padding)
  (setq comment-padding 0)
  ;(set (make-local-variable 'comment-start-skip) "/\\*+ *")
  (make-local-variable 'comment-end)
  (setq comment-end "*")
  ;(set (make-local-variable 'comment-end-skip) "\\*/")
  ;; Set things up for eldoc-mode.
  (set (make-local-variable 'eldoc-print-current-symbol-info-function)
       'clean-doc-current-info)
  ;; Set things up for imenu.
  (set (make-local-variable 'imenu-create-index-function)
       'clean-ds-create-imenu-index)
  ;; Set things up for font-lock.
  (set (make-local-variable 'font-lock-defaults)
       '(clean-font-lock-choose-keywords
         nil nil ((?\' . "w") (?_  . "w")) nil
         (font-lock-syntactic-keywords
          . clean-font-lock-choose-syntactic-keywords)
         (font-lock-syntactic-face-function
          . clean-syntactic-face-function)
         ;; Get help from font-lock-syntactic-keywords.
         (parse-sexp-lookup-properties . t)))
  ;(set (make-local-variable 'indent-tabs-mode) nil)
  ;(set (make-local-variable 'tab-width) 8)
  )

(defcustom clean-mode-hooks nil
  "Hook run after entering Clean mode."
  :type 'hook
  :options '(turn-on-clean-indent turn-on-font-lock turn-on-eldoc-mode
             imenu-add-menubar-index))

;; The main mode functions
;;;###autoload
(defun clean-mode ()
  "Major mode for editing Clean programs.

\\<clean-mode-map>\\[indent-for-comment] will place a comment at an appropriate place on the current line.
\\[comment-region] comments (or with prefix arg, uncomments) each line in the region.

Modules can hook in via `clean-mode-hook'.  The following modules
are supported with an `autoload' command:

   `clean-decl-scan', Graeme E Moss
     Scans top-level declarations, and places them in a menu.

   `clean-doc', Hans-Wolfgang Loidl
     Echoes types of functions or syntax of keywords when the cursor is idle.

   `clean-indent', Guy Lapalme
     Intelligent semi-automatic indentation.

   `clean-simple-indent', Graeme E Moss and Heribert Schuetz
     Simple indentation.

Module X is activated using the command `turn-on-X'.  For example,
`clean-font-lock' is activated using `turn-on-clean-font-lock'.
For more information on a module, see the help for its `turn-on-X'
function.  Some modules can be deactivated using `turn-off-X'.  (Note
that `clean-doc' is irregular in using `turn-(on/off)-clean-doc-mode'.)

Use `clean-version' to find out what version this is.

Invokes `clean-mode-hook' if not nil."

  (interactive)
  (clean-mode-generic))

;;;###autoload
(defun clean-mode-generic ()
  "Common part of `clean-mode'"

  (clean-vars)
  (setq major-mode 'clean-mode)
  (setq mode-name "Clean")
  (use-local-map clean-mode-map)
  (set-syntax-table clean-mode-syntax-table)
  (run-hooks 'clean-mode-hook))

;; Provide ourselves:

(provide 'clean-mode)

;;; clean-mode.el ends here

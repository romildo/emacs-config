;;; clean-font-lock.el --- Font locking module for Clean Mode    -*-coding: iso-8859-1;-*-

;; Copyright 2005 lethevert
;; Copyright 2003, 2004  Free Software Foundation, Inc.
;; Copyright 1997-1998 Graeme E Moss, and Tommy Thorn

;; Authors: 1997-1998 Graeme E Moss <gem@cs.york.ac.uk> and
;;                    Tommy Thorn <thorn@irisa.fr>
;;          2003  Dave Love <fx@gnu.org>
;;          2005  lethevert <lethevert@users.sourceforge.net>
;; Keywords: faces files Concurrent Clean
;; Version: 0.2
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
;; To support fontification of Concurrent Clean keywords, symbols,
;; functions, etc.
;;
;; Installation:
;; 
;; To turn font locking on for all Clean buffers under the Clean
;; mode of Moss&Thorn, add this to .emacs:
;;
;;    (add-hook 'clean-mode-hook 'turn-on-clean-font-lock)
;;
;; Otherwise, call `turn-on-clean-font-lock'.
;;
;;
;; Customisation:
;;
;; The colours and level of font locking may be customised.  See the
;; documentation on `turn-on-clean-font-lock' for more details.
;;
;;
;; History:
;;
;; Version 0.2
;;   Change reserved symbols.
;;
;; Version 0.1
;;   This mode is created by lethevert 2/7/2005 based on haskel-font-lock.
;;

;;; All functions/variables start with
;;; `(turn-(on/off)-)clean-font-lock' or `clean-fl-'.

;;; Code:

(eval-when-compile
  (require 'clean-mode)
  (require 'cl))
(require 'font-lock)

;; Version.
(defconst clean-font-lock-version "0.1"
  "Version number of clean-font-lock.")
(defun clean-font-lock-version ()
  "Echo the current version of clean-font-lock in the minibuffer."
  (interactive)
  (message "Using clean-font-lock version %s" clean-font-lock-version))

(defcustom clean-font-lock-symbols nil
  "Display \\ and -> and such using symbols in fonts.
This may sound like a neat trick, but be extra careful: it changes the
alignment and can thus lead to nasty surprises w.r.t layout.
If t, try to use whichever font is available.  Otherwise you can
set it to a particular font of your preference among `japanese-jisx0208'
and `unicode'."
  :group 'clean
  :type '(choice (const nil)
                 (const t)
                 (const unicode)
                 (const japanese-jisx0208)))

(defconst clean-font-lock-symbols-alist
  (append
   ;; The symbols can come from a JIS0208 font.
   (and (fboundp 'make-char) (charsetp 'japanese-jisx0208)
        (memq clean-font-lock-symbols '(t japanese-jisx0208))
        (list (cons "\\" (make-char 'japanese-jisx0208 38 75))
              (cons "->" (make-char 'japanese-jisx0208 34 42))
              (cons "<-" (make-char 'japanese-jisx0208 34 43))
              (cons "=>" (make-char 'japanese-jisx0208 34 77))))
   ;; Or a unicode font.
   (and (fboundp 'decode-char)
        (memq clean-font-lock-symbols '(t unicode))
        (list (cons "\\" (decode-char 'ucs 955))
              (cons "->" (decode-char 'ucs 8594))
              (cons "<-" (decode-char 'ucs 8592))
              (cons "=>" (decode-char 'ucs 8658))
              (cons "::" (decode-char 'ucs 8759))
              (cons "." (decode-char 'ucs 9675))))))

;; Use new vars for the font-lock faces.  The indirection allows people to
;; use different faces than in other modes, as before.
(defvar clean-keyword-face 'font-lock-keyword-face)
(defvar clean-constructor-face 'font-lock-type-face)
;; This used to be `font-lock-variable-name-face' but it doesn't result in
;; a highlighting that's consistent with other modes (it's mostly used
;; for function defintions).
(defvar clean-definition-face 'font-lock-function-name-face)
;; This is probably just wrong, but it used to use
;; `font-lock-function-name-face' with a result that was not consistent with
;; other major modes, so I just exchanged with `clean-definition-face'.
(defvar clean-operator-face 'font-lock-variable-name-face)
(defvar clean-default-face nil)

(defconst clean-emacs21-features (string-match "[[:alpha:]]" "x")
  "Non-nil if we have regexp char classes.
Assume this means we have other useful features from Emacs 21.")

(defun clean-font-lock-compose-symbol (alist)
  "Compose a sequence of ascii chars into a symbol.
Regexp match data 0 points to the chars."
  ;; Check that the chars should really be composed into a symbol.
  (let ((start (match-beginning 0))
        (end (match-end 0)))
    (if (or (memq (char-syntax (or (char-before start) ?\ )) '(?_ ?\\))
            (memq (char-syntax (or (char-after end) ?\ )) '(?_ ?\\))
            (memq (get-text-property start 'face)
                  '(font-lock-doc-face font-lock-string-face
                    font-lock-comment-face)))
        ;; No composition for you.  Let's actually remove any composition
        ;; we may have added earlier and which is now incorrect.
        (remove-text-properties start end '(composition))
      ;; That's a symbol alright, so add the composition.
      (compose-region start end (cdr (assoc (match-string 0) alist)))))
  ;; Return nil because we're not adding any face property.
  nil)

(defun clean-font-lock-symbols-keywords ()
  (when (fboundp 'compose-region)
    (let ((alist nil))
      (dolist (x clean-font-lock-symbols-alist)
        (when (and (if (fboundp 'char-displayable-p)
                       (char-displayable-p (cdr x))
                     t)
                   (not (assoc (car x) alist)))     ;Not yet in alist.
              (push x alist)))
      (when alist
        `((,(regexp-opt (mapcar 'car alist) t)
           (0 (clean-font-lock-compose-symbol ',alist))))))))

;; The font lock regular expressions.
(defun clean-font-lock-keywords-create ()
  "Create fontification definitions for Clean scripts.
Returns keywords suitable for `font-lock-keywords'."
  (let* (
         ;; a line of code starts with "^".
         (line-prefix "^")

         ;; Most names are borrowed from the lexical syntax of the Haskell
         ;; report.
         ;; Some of these definitions have been superseded by using the
         ;; syntax table instead.

         ;; (ASCsymbol "-!#$%&*+./<=>?@\\\\^|~")
         ;; Put the minus first to make it work in ranges.
         ;; (ISOsymbol "\241-\277\327\367")
         (ISOlarge  "\300-\326\330-\337")
         (ISOsmall  "\340-\366\370-\377")
         (small
          (if clean-emacs21-features "[:lower:]" (concat "a-z" ISOsmall)))
         (large
          (if clean-emacs21-features "[:upper:]" (concat "A-Z" ISOlarge)))
         (alnum
          (if clean-emacs21-features "[:alnum:]" (concat small large "0-9")))
         ;; (symbol
         ;;  (concat ASCsymbol ISOsymbol))

         ;; We allow _ as the first char to fit GHC
         (varid (concat "\\b[" small "_][" alnum "'_]*\\b"))
         (conid (concat "\\b[" large "][" alnum "'_]*\\b"))
         (modid (concat "\\b" conid "\\(\\." conid "\\)*\\b"))
         (qvarid (concat modid "\\." varid))
         (qconid (concat modid "\\." conid))
         (sym
          ;; We used to use the below for non-Emacs21, but I think the
          ;; regexp based on syntax works for other emacsen as well.  -- Stef
          ;; (concat "[" symbol ":]+")
          ;; Add backslash to the symbol-syntax chars.  This seems to
          ;; be thrown for some reason by backslash's escape syntax.
          "\\(\\s_\\|\\\\\\)+")

         ;; Reserved operations
         (reservedsym
          (concat "\\S_"
                  ;; (regexp-opt '(".." "::" "=" "\\\\" "|" "<-:" "<-" "->"
                  ;;                 "@" "~" "=>") t)
                  "\\(->\\|\\.\\.\\|::\\|<-:\\|<-\\|=>\\|\\\\\\\\\\|[=@\\|~]\\)"
                  "\\S_"))
         ;; Reserved identifiers
         (reservedid
          (concat "\\b"
                  ;; ?? `as' and `qualified' aren't in the Haskell98 list.
                  ;; `_' can go in here since it has temporary word syntax.
                  ;; (regexp-opt
                  ;;  '("_" "Bool" "case" "class" "Char" "definition" "derive" "do" "dynamic" "export"
                  ;;    "from" "False" "File" "generic"
                  ;;    "hiding" "if" "import" "implementation" "in" "infix" "infixl" "infixr" "instance"
                  ;;    "Int" "let" "module" "of" "otherwise" "Real" "special" "system" "String" "True"
                  ;;    "where" "with" "World") t)
                  "\\(_\\|Bool\\|c\\(ase\\|lass\\)\\|Char\\|d\\(e\\(finition\\|rive\\)\\|o\\|ynamic\\)\\|export"
                      "\\|from\\|F\\(alse\\|ile\\)\\|generic"
                      "\\|hiding\\|i\\(mp\\(ort\\|lementation\\)\\|n\\(fix[lr]?\\|stance\\)\\|[fn]\\)"
                      "\\|Int\\|let\\|module\\|o\\(f\\|therwise\\)\\|Real\\|s\\(pecial\\|ystem\\)\\|String\\|True"
                      "\\|w\\(here\\|ith\\)\\|World"
                  "\\)"
                  "\\b"))

         ;; This unreadable regexp matches strings and character
         ;; constants.  We need to do this with one regexp to handle
         ;; stuff like '"':"'".  The regexp is the composition of
         ;; "([^"\\]|\\.)*" for strings and '([^\\]|\\.[^']*)' for
         ;; characters, allowing for string continuations.
         ;; Could probably be improved...
         (string-and-char
         ;; (concat "\\(\\(\"\\|" line-prefix "[ \t]*\\\\\\)\\([^\"\\\\\n]\\|\\\\.\\)*\\(\"\\|\\\\[ \t]*$\\)\\|'\\([^'\\\\\n]\\|\\\\.[^'\n]*\\)'\\)"))
          (concat "\\(\\(\"\\|" line-prefix "[ \t]*\\\\\\)\\([^\"\\\\\n]\\|\\\\.\\)*\\(\"\\|\\\\[ \t]*$\\)"
                  "\\|\\(\'\\|" line-prefix "[ \t]*\\\\\\)\\([^\'\\\\\n]\\|\\\\.\\)*\\(\'\\|\\\\[ \t]*$\\)\\)"))

         ;; declarations
         (topdecl-var
          (concat line-prefix "[ \t]*\\(" varid "\\)\\s-*\\("
                  varid "\\|" conid "\\|::\\|=\\||\\|\\s(\\)"))
         (topdecl-var2
          (concat line-prefix "[ \t]*\\(" varid "\\|" conid "\\)\\s-*`\\(" varid "\\)`"))
         (topdecl-sym
          (concat line-prefix "[ \t]*\\(" varid "\\|" conid "\\)\\s-*\\(" sym "\\)"))
         (topdecl-sym2 (concat line-prefix "[ \t](\\(" sym "\\))"))

         keywords)

    (setq keywords
          `(;; NOTICE the ordering below is significant
            ;;
            ("^#.*$" 0 'font-lock-warning-face t)
            ,@(unless clean-emacs21-features
                ;; Expensive.
                `((,string-and-char 1 font-lock-string-face)))

            ;; This is "still-thinking" mark
            ("\\?\\?" 0 (symbol-value 'font-lock-warning-face))

            ;("::.*$" 0 (symbol-value 'clean-constructor-face))

            (,reservedid 1 (symbol-value 'clean-keyword-face))
            (,reservedsym 1 (symbol-value 'clean-operator-face))

            ;; Declarations.
            ;; Place them *before* generic id-and-op highlighting.
            (,topdecl-var  (1 (symbol-value 'clean-definition-face)))
            (,topdecl-var2 (2 (symbol-value 'clean-definition-face)))
            (,topdecl-sym  (2 (symbol-value 'clean-definition-face)))
            (,topdecl-sym2 (1 (symbol-value 'clean-definition-face)))

            ;; These four are debatable...
            ("(\\(,*\\|->\\))" 0 (symbol-value 'clean-constructor-face))
            ("\\[\\]" 0 (symbol-value 'clean-constructor-face))
            ;; Expensive.
            (,qvarid 0 clean-default-face)
            (,qconid 0 (symbol-value 'clean-constructor-face))
            (,(concat "\`" varid "\`") 0 (symbol-value 'clean-operator-face))
            ;; Expensive.
            (,conid 0 (symbol-value 'clean-constructor-face))

            ;; Very expensive.
            (,sym 0 (if (eq (char-after (match-beginning 0)) ?:)
                        clean-constructor-face
                      clean-operator-face))
            ,@(clean-font-lock-symbols-keywords)))
    keywords))

(defconst clean-basic-syntactic-keywords
  '(;; Character constants (since apostrophe can't have string syntax).
    ;; Beware: do not match something like 's-}' or '\n"+' since the first '
    ;; might be inside a comment or a string.
    ;; This still gets fooled with "'"'"'"'"'"', but ... oh well.
    ("\\Sw\\('\\)\\([^\\'\n]\\|\\\\.[^\\'\n \"}]*\\)\\('\\)" (1 "|") (3 "|"))
    ;; The \ is not escaping in \(x,y) -> x + y.
    ("\\(\\\\\\)(" (1 "."))
    ;; The second \ in a gap does not quote the subsequent char.
    ;; It's probably not worth the trouble, tho.
    ;; ("^[ \t]*\\(\\\\\\)" (1 "."))
    ;; Deal with instances of `--' which don't form a comment.
    ("\\s_\\{3,\\}" (0 (if (string-match "\\`-*\\'" (match-string 0))
                           nil           ; Sequence of hyphens.  Do nothing in
                                         ; case of things like `{---'.
                         "_")))))        ; other symbol sequence

(defun clean-syntactic-face-function (state)
  "`font-lock-syntactic-face-function' for Clean."
  (cond
   ((nth 3 state) font-lock-string-face)                ; as normal
   (t font-lock-comment-face)))

(defconst clean-font-lock-keywords
  (clean-font-lock-keywords-create)
  "Font lock definitions for Clean.")

(defun clean-font-lock-choose-keywords ()
  clean-font-lock-keywords)

(defun clean-font-lock-choose-syntactic-keywords ()
  clean-basic-syntactic-keywords)

(defun clean-font-lock-defaults-create ()
  "Locally set `font-lock-defaults' for Clean."
  (set (make-local-variable 'font-lock-defaults)
       '(clean-font-lock-choose-keywords
         nil nil ((?\' . "w") (?_  . "w")) nil
         (font-lock-syntactic-keywords
          . clean-font-lock-choose-syntactic-keywords)
         (font-lock-syntactic-face-function
          . clean-syntactic-face-function)
         ;; Get help from font-lock-syntactic-keywords.
         (parse-sexp-lookup-properties . t))))

;; The main functions.
(defun turn-on-clean-font-lock ()
  "Turns on font locking in current buffer for Clean scripts.

Changes the current buffer's `font-lock-defaults', and adds the
following variables:

   `clean-keyword-face'      for reserved keywords and syntax,
   `clean-constructor-face'  for data- and type-constructors, class names,
                               and module names,
   `clean-operator-face'     for symbolic and alphanumeric operators,
   `clean-default-face'      for ordinary code.

The variables are initialised to the following font lock default faces:

   `clean-keyword-face'      `font-lock-keyword-face'
   `clean-constructor-face'  `font-lock-type-face'
   `clean-operator-face'     `font-lock-function-name-face'
   `clean-default-face'      <default face>

Two levels of fontification are defined: level one (the default)
and level two (more colour).  The former does not colour operators.
Use the variable `font-lock-maximum-decoration' to choose
non-default levels of fontification.  For example, adding this to
.emacs:

  (setq font-lock-maximum-decoration '((clean-mode . 2) (t . 0)))

uses level two fontification for `clean-mode' and default level for
all other modes.  See documentation on this variable for further
details.

To alter an attribute of a face, add a hook.  For example, to change
the foreground colour of comments to brown, add the following line to
.emacs:

  (add-hook 'clean-font-lock-hook
      (lambda ()
          (set-face-foreground 'clean-comment-face \"brown\")))

Note that the colours available vary from system to system.  To see
what colours are available on your system, call
`list-colors-display' from emacs.

To turn font locking on for all Clean buffers, add this to .emacs:

  (add-hook 'clean-mode-hook 'turn-on-clean-font-lock)

To turn font locking on for the current buffer, call
`turn-on-clean-font-lock'.  To turn font locking off in the current
buffer, call `turn-off-clean-font-lock'.

Invokes `clean-font-lock-hook' if not nil.

Use `clean-font-lock-version' to find out what version this is."

  (interactive)
  (clean-font-lock-defaults-create)
  (run-hooks 'clean-font-lock-hook)
  (turn-on-font-lock))

(defun turn-off-clean-font-lock ()
  "Turns off font locking in current buffer."
  (interactive)
  (font-lock-mode -1))

;;; Provide ourselves:

(provide 'clean-font-lock)

;;; clean-font-lock.el ends here

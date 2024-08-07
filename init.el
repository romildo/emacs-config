;;; init.el --- Personal emacs initialization -*- lexical-binding: t -*-

;; Copyright (c) 2019-2023  José Romildo Malaquias <malaquias@gmail.com>

;; Author: José Romildo Malaquias <malaquias@gmail.com>
;; Version: 0.1.1
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;;; Code:

;;;; See https://github.com/a13/emacs.d for ideas

(when (< emacs-major-version 27)
  (message "Your Emacs is old, and some functionality in this config will be disabled.")
  (message "Please upgrade if possible."))

(message "load-path: %s" load-path)

(dolist (subdir '("lisp" "lib"))
  (let ((dir (expand-file-name subdir user-emacs-directory)))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

;; ----------------------------------------------------------------------
;; Benchamarking initialization
;; ----------------------------------------------------------------------

;; (defun jrm/time-subtract-millis (b a)
;;   "Ellapsed time between B and A in milliseconds."
;;   (* 1000.0 (float-time (time-subtract b a))))

;; (defvar jrm/require-times nil
;;   "A list of (FEATURE . LOAD-DURATION).
;; LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

;; (defadvice require (around jrm/build-require-times (feature &optional filename noerror) activate)
;;   "Note in `jrm/require-times' the time taken to require each feature."
;;   (let* ((already-loaded (memq feature features))
;;          (require-start-time (and (not already-loaded) (current-time))))
;;     (prog1
;;         ad-do-it
;;       (when (and (not already-loaded) (memq feature features))
;;         (let ((time (jrm/time-subtract-millis (current-time) require-start-time)))
;;           (add-to-list 'jrm/require-times (cons feature time) t))))))

;;;;;;;;;;


(if (require 'benchmark-init nil 'noerror)
    (progn
      (message "benchmark-init loaded")
      (benchmark-init/activate)
      (add-hook 'after-init-hook (lambda ()
                                   ;; (benchmark-init/show-durations-tabulated)
                                   ;; (benchmark-init/show-durations-tree)
                                   (benchmark-init/deactivate)
                                   )))
  (message "benchmark-init not loaded yet"))


;;----------------------------------------------------------------------------
;; Packages
;;----------------------------------------------------------------------------

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("user42" . "https://download.tuxfamily.org/user42/elpa/packages/") t)

;; Loading Emacs Lisp packages, and activating them is needed only if
;; package-enable-at-startup is disabled in early-init
;; (package-initialize)
;; (package-initialize 'no-activate) ; ?????

(setq package-name-column-width 36)

(add-hook 'package-menu-mode-hook #'hl-line-mode)

(global-set-key [(control c) ?p] 'list-packages) ; display a list of packages

(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t)

(use-package auto-package-update
  ;; Automatically update Emacs packages at startup
  :ensure
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  (auto-package-update-interval 2)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-show-preview t)
  :hook
  (auto-package-update-before . (lambda () (message "Starting auto-package-update...")))
  (auto-package-update-after . (lambda () (message "End auto-package-update.")))
  ;; (after-init . auto-package-update-maybe)
  :config
  ;; (auto-package-update-maybe)
  :bind (("C-c P" . auto-package-update-now-async)))

;;----------------------------------------------------------------------------

(use-package benchmark-init
  ;; Benchmark emacs initialization
  :ensure
  ;; :demand
  ;; :disabled
  ;; :hook (after-init . benchmark-init/deactivate)
  )

(use-package async
  :ensure
  :config
  ;; Enable asynchronous compilation of elpa packages
  (async-bytecomp-package-mode 1))

;;----------------------------------------------------------------------------

(use-package bind-key :ensure) ;; A simple way to manage personal keybindings

(use-package delight
  ;; Enables customisation of mode names displayed in the mode line
  ;;:disabled
  :ensure
  ;; :defer
  :config
  (delight
   '(
     ;; <mode symbol> <replacement> <library>
     ;; (abbrev-mode " Abv" abbrev)
     ;; (smart-tab-mode " \\t" smart-tab)
     ;; (eldoc-mode nil "eldoc")
     ;; (rainbow-mode)
     ;; (overwrite-mode " Ov" t)
     ;; (emacs-lisp-mode "Elisp" :major)
     ))
  
  ;; (scheme-mode "λ")
  ;; (help-mode "🄷")
  ;; (isearch-mode " 🔎")
  ;; (view-mode " 👀" 'view)
  ;; (emacs-lisp-mode           "EL")
  ;; (inferior-emacs-lisp-mode  "EL>")
  ;; (calendar-mode             "📆")
  ;; (visual-line-mode   " ↩")
  ;; (auto-fill-function " ↵")
  ;; (eldoc-mode         ""    eldoc)
  ;; (whitespace-mode    " _"  whitespace)
  ;; (paredit-mode       " ()" paredit)
  )


;; my own custom key map which later is bound to my own minor mode
(defvar my/mode-map (make-keymap)
  "A keymap for custom bindings.")


;;----------------------------------------------------------------------------
;; Some basic preferences
;;----------------------------------------------------------------------------

(use-package emacs
  ;; These were defined in C code, so use a pseudo-package to set them

  :init
  ;; Enable some disabled commands. They are disabled by default
  ;; because new users often find them confusing.
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'narrow-to-region 'disabled nil) ;; Restrict editing to the current region

  :custom
  (hscroll-step 1 "How many columns to scroll the window when point gets too close to the edge")
  ;; (enable-recursive-minibuffers t "Allow minibuffer commands while in the minibuffer")
  (indent-tabs-mode nil "Indentation cannot insert tabs")
  (truncate-partial-width-windows nil)
  (scroll-preserve-screen-position 'always)
  (visible-bell t "try to flash the frame to represent a bell")
  ;; (truncate-lines nil "do not truncate long lines instead of line wrapping them")

  ;; How many columns away from the window edge point is allowed to get
  ;; before automatic hscrolling will horizontally scroll the window.
  (hscroll-margin 1)

  ;; http://www.emacswiki.org/cgi-bin/wiki?SmoothScrolling
  (scroll-conservatively 10000)

  ;; (show-trailing-whitespace t "highlight trailing whitespace")

  ;; Visually indicate empty lines after the buffer end.
  ;; If non-nil, a bitmap is displayed in the left fringe of a window on
  ;; window-systems.
  (indicate-empty-lines t)

  (frame-title-format
   '("%S: " (buffer-file-name "%f" (dired-directory dired-directory "%b"))))

  (initial-scratch-message nil)
  )

(use-package frame
  ;; Disable suspending the current frame on C-z
  :custom
  (blink-cursor-interval 0.4 "length of cursor blink interval in seconds")
  
  :bind ("C-z" . nil))

(use-package mouse
  :custom
  (mouse-yank-at-point t))

(use-package bookmark
  :custom
  (bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)))

(use-package apropos
  :custom
  (apropos-do-all t))

(use-package delsel
  :config
  ;; delete the selection instead of inserting at point
  (delete-selection-mode))

(use-package cua-base
  ;; Common User Access style editing (CUA mode)
  :config
  ;; (cua-mode)
  :custom
  (cua-auto-tabify-rectangles nil "Don't tabify after rectangle commands")
  (cua-keep-region-after-copy t "Standard Windows behaviour"))

(use-package fringe
  ;; By default both fringes have width 8 pixels, but we can easily adjust this.
  :disabled
  :config
  (fringe-mode nil) ; restore default: 8 pixels
  (set-face-attribute 'fringe nil :foreground "Black" :background "Wheat"))

(use-package misc ; some nonstandard editing and utility commands
  :bind ([remap zap-to-char] . zap-up-to-char)) ; M-z

(use-package tool-bar
  ;; Disables the toolbar
  :config (tool-bar-mode -1))

(use-package menu-bar
  :custom (buffers-menu-max-size 30 "maximum number of entries on the Buffers menu")
  :config (menu-bar-mode -1)
  :bind ([S-f10] . menu-bar-mode))

(use-package font-lock
  :config
  (setq-default font-lock-maximum-decoration t
                font-lock-maximum-size nil)
  (global-font-lock-mode 1))

(use-package simple ; basic editing commands
  :config
  ;; Toggle visual line based editing. In visual line mode ordinary line
  ;; continuation uses word wrap: each long logical line is divided into one
  ;; or more screen lines, like in ordinary line continuation.  However, Emacs
  ;; attempts to wrap the line at word boundaries near the right window
  ;; edge. This makes the text easier to read, as wrapping does not occur in
  ;; the middle of words. Some editing commands work on screen lines instead
  ;; of logical lines.
  
  ;; (global-visual-line-mode 1)

  :custom
  (line-move-visual nil "Line motion by logical lines based on buffer contents alone")

  ;; Save clipboard strings into kill ring before replacing them.
  ;; When one selects something in another program to paste it into
  ;; Emacs, but kills something in Emacs before actually pasting it,
  ;; this selection is gone unless this variable is non-nil, in which
  ;; case the other program's selection is saved in the `kill-ring'
  ;; before the Emacs kill and one can still paste it using C-y M-y.
  (save-interprogram-paste-before-kill t "Save existing clipboard text into kill ring before replacing it")

  (set-mark-command-repeat-pop t "repeating C-SPC after popping mark pops it again")

  ;; highlight the locus indefinitely until some other locus replaces it
  (next-error-highlight t)
  (next-error-highlight-no-select t)
  (next-error-recenter -2)
  )

(use-package display-line-numbers
  :custom-face
  :config
  ;; maybe use custom-face ?
  (set-face-attribute 'line-number nil
                      ;; :family "Monospace"
                      :width 'extra-condensed
                      :height 0.75
                      :weight 'thin
                      :slant 'oblique
                      ;; :inverse-video t
                      )
  (set-face-attribute 'line-number-current-line nil
                      :inherit 'line-number
                      :weight 'semi-bold)
  :hook
  (after-init . global-display-line-numbers-mode))

(use-package hl-line
  ;; Toggle line highlighting in all buffers
  :disabled
  :config
  (global-hl-line-mode 1))

(use-package files
  :custom
  (require-final-newline t "Always end a file with a newline"))

;; (use-package grep
;;   :custom
;;   (grep-highlight-matches t "Use special markers to highlight grep matches"))

(use-package recentf
  ;; keep a list of recently opened files
  :if (not noninteractive)
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 512)
  )

(use-package saveplace
  ;; Automatically save place in files, so that visiting them later
  ;; (even during a different Emacs session) automatically moves point
  ;; to the saved position, when the file is first found.
  :init
  (save-place-mode 1))

(use-package hippie-expand
  :bind ("M-/" . hippie-expand))

(use-package notifications ; desktop notifications
  :defer)

;; needed since I started using archlinux
(use-package iso-transl ; keyboard input definitions for ISO 8859-1
  :disabled t
  :defer)

(use-package smart-newline
  ;; Provide smart newline for one keybind.
  ;; https://github.com/ainame/smart-newline.el
  :ensure
  :config
  (smart-newline-mode 1))

(use-package whitespace
  ;; Minor mode to visualize whitepsaces (TAB, (HARD) SPACE and NEWLINE)
  ;; To clean up trailing whitespace, you can also run whitespace-cleanup command.
  :bind
  ([(control c) ?w] .  whitespace-mode) ;; toggle whitespace locally
  
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'whitespace-mode))
  
  :config
  (setq whitespace-line-column nil)
  (setq whitespace-style '()))


(use-package elec-pair			; automatic bracket insertion by pairs
  :defer
  :init (add-hook 'emacs-lisp-mode-hook #'electric-pair-local-mode)
  ;; :config (electric-pair-mode 1)
  )

(use-package smartparens-config
  :ensure smartparens
  :disabled ; REMOVE LATER
  ;;:delight smartparens-mode
  :config
  (smartparens-global-mode 1)
  ;; (show-smartparens-global-mode 1)
  ;; (setq
  ;;  smartparens-strict-mode nil
  ;;  sp-autoinsert-if-followed-by-word nil
  ;;  sp-autoskip-closing-pair 'always
  ;;  sp-base-key-bindings 'paredit
  ;;  sp-hybrid-kill-entire-symbol nil)
  )

(use-package savehist
  ;; Save minibuffer history
  :custom
  (history-length 1000 "Maximum length of history lists before truncation takes place")
  :config
  (savehist-mode))

(use-package mb-depth
  ;; Indicate minibuffer depth
  :disabled
  :config
  ;; Any recursive use of the minibuffer will show the recursion depth
  ;; in the minibuffer prompt
  (minibuffer-depth-indicate-mode))

(use-package time-stamp
  :hook before-save)

(use-package desktop
  :disabled
  :config
  ;; Enable Desktop Save mode
  ;;
  ;; Emacs will save the desktop when it exits (this may prompt you; see
  ;; the option ‘desktop-save’).  The next time Emacs starts, if this
  ;; mode is active it will restore the desktop.
  ;; 
  ;;(desktop-save-mode 1)
  )

(use-package "window"
  :custom
  (split-height-threshold 160 "minimum height for splitting windows sensibly")
  (split-width-threshold 80 "minimum width for splitting windows sensibly")
  )

(use-package re-builder
  :defer
  ;; :bind (("C-c R" . re-builder))
  :custom
  (reb-re-syntax 'string "syntax for REs in the RE Builder: read, string or rx"))

(use-package window-divider-mode
  :hook
  (after-init . window-divider-mode)
  :custom
  (window-divider-default-right-width 2)
  (window-divider-default-bottom-width 2)
  (window-divider-default-places t))

;;; ---------------------------------------------------------------------------

(use-package goto-last-point
  ;; Record and jump to the last point in the buffer
  :ensure
  :bind ("C-<" . goto-last-point)
  :config (goto-last-point-mode))

(use-package default-text-scale
  ;; Easily adjust the font size in all Emacs frames
  :ensure
  :defer
  ;; :bind (("C-M-=" . default-text-scale-increase)
  ;;        ("C-M--" . default-text-scale-decrease))
  )

(use-package iqa
  ;; Init file (and directory) Quick Access
  ;;   C-x M-f — iqa-find-user-init-file
  ;;   C-x M-r — iqa-reload-user-init-file
  ;;   C-x M-d — iqa-find-user-init-directory
  :ensure
  :config
  (iqa-setup-default))

(defun my/find-user-init-file ()
  "Open the user's init file."
  (interactive)
  (find-file user-init-file))


(defun my/delete-whitespace ()
  "Delete all whitespace characters (spaces, tabs, newlines,
carrage returns) around point."
  (interactive "*")
  (save-excursion
    (save-restriction
      (delete-region
       (progn (skip-chars-backward " \t\n\r") (point))
       (progn (skip-chars-forward " \t\n\r") (point))))))

(global-set-key [(super \\)] 'my/delete-whitespace)

(defun my/just-one-space (&optional n)
  "Delete all whitespace characters (including newlines) around
point, leaving one space (or N spaces).  See also `just-one-space'."
  (interactive "*p")
  (just-one-space (- n)))

(global-set-key [(meta ? )] #'my/just-one-space)

;; M-SPC    just-one-space	     delete spaces and tabs around point,
;;                                   leaving one space
;; M-\	    delete-horizontal-space  delete spaces and tabs around point
;; M-^	    delete-indentation       join two lines by deleting the intervening newline,
;;                                   along with any indentation following it
;; C-x C-o  delete-blank-lines       delete blank lines around the current line
;;          cycle-spacing            manipulate whitespace around point in a smart way


(global-set-key [(control x) ?p]
                (lambda ()
                  (interactive)
                  (let ((buf (get-buffer "*Packages*")))
                    (if buf
                        (switch-to-buffer buf)
                      (list-packages)))))

(global-set-key [f2] 'grep)
(global-set-key [(shift f2)] 'grep-find)
(global-set-key [(control f2)] 'find-grep-dired)
(global-set-key [(meta f2)] 'find-dired)

(global-set-key [(control c) ?e] 'first-error)  ;; Also C-u M-g M-n; ...
(global-set-key [(meta down)] 'next-error)	;; Also M-g M-n; M-g n; C-x `
(global-set-key [(meta up)] 'previous-error)	;; Also M-g M-p; M-g p

;; (grep-apply-setting 'grep-find-command
;;   '("find . -type f | sort | xargs grep -rnH -i -e " . 15))


;; WindowResize
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)


;; (global-set-key [f3] 'find-file)

(global-set-key [f4] 'make-frame)
(global-set-key [(control f4)] 'delete-frame)

;; revert buffers when files on disk change
;; (global-auto-revert-mode 1)


(defun revert-buffer-no-confirm-if-not-modified ()
  "Revert buffer prompting for confirmation only if the buffer is
modified."
  (interactive)
  (revert-buffer t (not (buffer-modified-p)) t))

(global-set-key [f5] 'revert-buffer-no-confirm-if-not-modified)

;; (global-set-key [f6] 'next-error)
;; (global-set-key [(shift f6)] 'previous-error)

(global-set-key [f7] 'save-buffer)
;;(global-set-key [f8] "\C-x\C-s\C-x0") ; 'save-file' followed by 'delete-window'

(global-set-key [f9] 'ispell-buffer)

(global-set-key [(control backspace)] 'undo)

;; Better than the standard bindings `C-x <right>' and `C-x <left>',
;; because you can hold these down to repeat: cycle through buffers.
;; (global-set-key [C-pause] 'previous-buffer)
;; (global-set-key [M-pause] 'next-buffer)


; When editing C code (and Lisp code and the like), I often
; like to insert tabs into comments and such.  It gets to be
; a pain to always have to use `C-q TAB', so I set up a more
; convenient binding.  Note that this does not work in
; TTY frames, where tab and shift-tab are indistinguishable.

;(define-key global-map '(shift tab) 'self-insert-command)


;; pre-defined keys for scaling font size:
;; C-x C--  or  C-x -
;; C-x C-+  or  C-x +  or  C-x C-=  or  C-x -
;; C-x C-0  or  C-x 0

;; pre-defined keys for scaling font size globally:
;; C-x C-M--
;; C-x C-M-+  or  C-x C-M=
;; C-x C-M-0


(global-set-key [(control x) ?a ?r] 'align-regexp)
(global-set-key [(meta \[)] 'align)


;; Move point N words to the right (to the left if N is negative),
;; stopping at the beginning of the word
;; It was originally bound to right-word
(global-set-key [M-right] 'forward-to-word)


;; Open terminal from emacs
(let ((my/terminal "terminology"))
  (when (executable-find my/terminal)
    (global-set-key [(control x) ?t] (kbd (concat "M-! " my/terminal " RET")))))

(use-package terminal-here
  ;; open an external terminal emulator in the current directory
  :ensure
  :bind (([(control c) ?o ?t] . terminal-here-launch)
         ([(control c) ?o ?p] . terminal-here-project-launch))
  :config
  (defun my/gsettings-get (schema key)
    (when (executable-find "gsettings")
      (let ((value (shell-command-to-string (concat "gsettings get " schema " " key))))
        (when (not (equal value (concat "No such key '" key "'\n")))
          (substring value 1 -2)))))

  :custom
  (terminal-here-terminal-command
   (cond
    ((terminal-here--pick-linux-default))
    ((let ((term (my/gsettings-get "org.gnome.desktop.default-applications.terminal" "exec")))
       (and term (list term))))
    ((executable-find "terminology") (list "terminology"))
    ((executable-find "qterminal") 'qterminal)
    ((executable-find "x-terminal-emulator") 'x-terminal-emulator)
    ((executable-find "xfce4-terminal") (lambda (dir) (list "xfce4-terminal" (concat "--working-directory=" dir))))
    ((executable-find "gnome-terminal") 'gnome-terminal)
    ((executable-find "xterm") 'xterm))))

;; Windmove is a library built into GnuEmacs starting with version
;; 21. It lets you move point from window to window using Shift and the
;; arrow keys. This is easier to type than ‘C-x o’ and, for some users,
;; may be more intuitive.
;; http://emacswiki.org/emacs/WindMove

;; (windmove-default-keybindings 'meta)

;; Package: clean-aindent-mode
;; When you press RET to create a newline and got indented by
;; eletric-indent-mode, you have appropriate whitespace for
;; indenting. But, if you leave the line blank and move to the next
;; line, the whitespace becomes useless. This package helps clean up
;; unused whitespace.
;; (when (fboundp 'clean-aindent-mode)
;;   (add-hook 'prog-mode-hook 'clean-aindent-mode))
 

(use-package rainbow-delimiters
  ;; Highlight nested parentheses, brackets, and braces according to their depth.
  :ensure
  :diminish
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package evil-search-highlight-persist
  ;; Persistent highlights after search
  :disabled ; REMOVE LATER
  :ensure
  :config
  (global-evil-search-highlight-persist t))


;; http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;; http://stackoverflow.com/questions/2416655/file-path-to-clipboard-in-emacs
(defun copy-buffer-file-name-as-kill (choice)
  "Copyies the buffer {name/mode}, file {name/full path/directory} to the kill-ring."
  (interactive "cCopy (b) buffer name, (m) buffer major mode, (f) full buffer-file path, (d) buffer-file directory, (n) buffer-file basename")
  (let ((new-kill-string)
        (name (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                (or (buffer-file-name) ""))))
    (cond ((eq choice ?f)
           (setq new-kill-string name))
          ((eq choice ?d)
           (setq new-kill-string (file-name-directory name)))
          ((eq choice ?n)
           (setq new-kill-string (file-name-nondirectory name)))
          ((eq choice ?b)
           (setq new-kill-string (buffer-name)))
          ((eq choice ?m)
           (setq new-kill-string (format "%s" major-mode)))
          (t (message "Quit")))
    (when new-kill-string
      (message "%s copied" new-kill-string)
      (kill-new new-kill-string))))


;; http://unix.stackexchange.com/questions/19494/how-to-colorize-text-in-emacs
;; this seems to  be very slow for large files
(define-derived-mode my/fundamental-ansi-mode fundamental-mode "fundamental ansi"
  "Fundamental mode that understands ansi colors."
  (require 'ansi-color)
  (ansi-color-apply-on-region (point-min) (point-max)))

;; (add-to-list 'auto-mode-alist '("nix-build.log\\'" . my/fundamental-ansi-mode))


;; https://stackoverflow.com/questions/23378271/how-do-i-display-ansi-color-codes-in-emacs-for-any-mode/23382008
(use-package tty-format
  ;; Adds Emacs format-alist entries for two tty oriented text
  ;; annotations: ANSI SGR escape sequences, and backspace
  ;; bold,underline and overline.
  ;; http://user42.tuxfamily.org/tty-format/index.html

  :ensure tty-format ; available on "user42" ELPA archive (not working?)
  ;; :load-path "lib"

  :config
  ;; M-x display-ansi-colors to explicitly decode ANSI color escape sequences
  (defun display-ansi-colors ()
    (interactive)
    (format-decode-buffer 'ansi-colors))

  ;; decode ANSI color escape sequences for *.txt or README files
  (add-hook 'find-file-hooks 'tty-format-guess)

  ;; decode ANSI color escape sequences for .log files
  (add-to-list 'auto-mode-alist '("\\.log\\'" . display-ansi-colors))
  )


;; www.emacswiki.org/emacs/SortWords
;; (Tip: use C-u C-x = to display the category of the character under the cursor.)

(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
Prefixed with negative \\[universal-argument], sorts in reverse.
  
The variable `sort-fold-case' determines whether alphabetic case
affects the sort order.
  
See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

(defun sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
See `sort-words'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))



;; http://www.emacswiki.org/emacs/KillingBuffers

;; Close and Kill Buffer in a Pane

;; Many times a temporary window comes up for help or compilation
;; results, auto complete, buffer list, etc. To close that one has to
;; switch to the other window and kill the buffer. After killing the
;; buffer, another buffer gets shown in that window and so we need to
;; close the window to restore the original buffer with full size.

;; These two functions solve this problem. You can bind your un-used
;; keys to these functions for quick access.

(defun close-and-kill-this-pane ()
  "If there are multiple windows, then close this pane and kill the buffer in it also."
  (interactive)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

(defun close-and-kill-next-pane ()
  "If there are multiple windows, then close the other pane and kill the buffer in it also."
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

(global-set-key [(super k)] 'close-and-kill-this-pane)

;;; ---------------------------------------------------------------------------
;;; mode-line stuff

(defun my/mode-line-config ()
  (unless (facep 'display-time-mail-face)
    (defface display-time-mail-face '((t (:background "red")))
      "If display-time-use-mail-icon is non-nil, its background
      colour is that of this face. Should be distinct from
      mode-line. Note that this does not seem to affect
      display-time-mail-string as claimed."))

  ;; Enable display of time, load level, and mail flag in mode lines.
  (setq display-time-format "%T")
  ;; (setq display-time-day-and-date t)
  ;; (setq display-time-24hr-format t)
  ;; (setq display-time-use-mail-icon t)
  ;; (setq display-time-mail-string "Mail")
  (setq display-time-interval 1)
  (setq
   display-time-string-forms
   '((if (and (not display-time-format) display-time-day-and-date)
         (format-time-string "%a %b %e " now)
       "")
     (propertize
      (format-time-string (or display-time-format
                              (if display-time-24hr-format "%H:%M" "%-I:%M%p"))
                          now)
      'face 'mode-line-emphasis
      'help-echo (format-time-string "%T %a %b %e, %Y (%Z)" now))
     load
     (if mail
         (concat
          " "
          (propertize
           display-time-mail-string
           'display `(when (and display-time-use-mail-icon
                                (display-graphic-p))
                       ,@display-time-mail-icon
                       ,@(if (and display-time-mail-face
                                  (memq (plist-get (cdr display-time-mail-icon) :type)
                                        '(pbm xbm)))
                             (let ((bg (face-attribute display-time-mail-face :background)))
                               (if (stringp bg)
                                   (list :background bg)))))
           'face display-time-mail-face
           'help-echo "You have new mail\nmouse-2: Read mail"
           'mouse-face 'mode-line-highlight
           'local-map (make-mode-line-mouse-map 'mouse-2 read-mail-command)))
       "")))
  )

(set-face-attribute 'mode-line nil
                    :inherit 'variable-pitch
                    :foreground "black"
                    :background "grey75"
                    :box '(:line-width -1 :style nil)
                    :height 0.8
                    )

;; (set-face-attribute 'mode-line-inactive nil
;;                     :inherit 'variable-pitch
;;                     :foreground "black"
;;                     :background "grey80"
;;                     :box '(:line-width -1 :color "grey40" :style nil)
;;                     :height 0.8
;;                     :weight 'light
;;                     )

(display-time-mode) ; display time, load level, and mail flag in mode lines

(column-number-mode) ; display column number in mode lines

;; (size-indication-mode) ; display buffer size in mode lines

;; (display-battery-mode) ; display battery status in mode lines
;; (setq battery-mode-line-format "[%b%p%% %t,%d°C]")

(use-package simple-modeline
  ;; A simple mode-line configuration for Emacs.
  :ensure
  :disabled
  :hook (after-init . simple-modeline-mode))

(use-package telephone-line
  ;; A new implementation of Powerline for Emacs
  ;; Requires the fonts included with all-the-icons
  :ensure
  :disabled
  :config
  (setq telephone-line-lhs
        '((accent . (telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-minor-mode-segment
                     telephone-line-buffer-segment))))
  (setq telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))))
  (telephone-line-mode t))

(use-package doom-modeline
  ;; A fancy and fast mode-line inspired by minimalism design.
  ;; It is able to display icons if nerd-icons package and required fonts are installed.
  :ensure
  :hook
  (after-init . doom-modeline-mode)
  :custom
  ;; (doom-modeline-buffer-file-name-style 'file-name)
  ;; (doom-modeline-height 24 "How tall the mode-line should be. Default: 23")
  ;; (doom-modeline-bar-width 6 "How wide the mode-line bar should be. Default: 4 ?")
  (doom-modeline-minor-modes t "Whether display the minor modes in the mode-line. Default: nil"))

(use-package nerd-icons
  ;; A library for easily using Nerd Font icons inside Emacs, an alternative to all-the-icons.
  ;; It works on both GUI and terminal! You only need a Nerd Font installed on your system.
  ;; It is inspired by all-the-icons, icons-in-terminal, vim-devicons, and nvim-web-devicons.
  :ensure
  :defer
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  (nerd-icons-font-family "FantasqueSansM Nerd"))

(use-package smart-mode-line
  ;; A powerful and beautiful mode-line for Emacs
  :ensure
  :disabled
  :config
  ;; (setq sml/theme 'respectful)
  ;; (sml/setup)
  )

(use-package smart-mode-line-atom-one-dark-theme
  ;; An atom-one-dark theme for smart-mode-line
  :ensure
  :disabled
  :after smart-mode-line
  :config
  (setq sml/theme 'atom-one-dark)
  (sml/setup))

(use-package uniquify ; make buffer names unique
  :custom
  (uniquify-buffer-name-style 'post-forward)
  (uniquify-ignore-buffers-re "^\\*")
  (uniquify-strip-common-suffix nil))

;;(require 'dircolors nil 'noerror)

(use-package eyebrowse ; a simple-minded way of managing window configuration
  :ensure
  :disabled
  :config
  (eyebrowse-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;; HERE
;; (use-package newcomment
;;   ;; (un)comment regions of buffers
;;   :bind (("s-/" . comment-or-uncomment-region))
;;   :config
;;   )

(use-package niceify-info
  ;; Improve styles and cross-references in Emacs Info buffers.
  :ensure
  :defer
  :hook (Info-selection . niceify-info))

;;; ---------------------------------------------------------------------------

(use-package yaml-mode
  ;; major mode for editing files in the YAML data serialization format
  :ensure
  :defer
  ;; :mode ("\\.yaml\\'" "\\.yml\\'")
  ;; :hook (yaml-mode . '(lambda ()
  ;;                       (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
  )

;; (use-package flycheck-yamllint
;;   :ensure
;;   :after yaml-mode)

;;; ---------------------------------------------------------------------------

(use-package magit
  ;; an interface to the version control system Git
  :ensure
  ;; :delight (magit-status-mode (propertize (concat " " [#xF1D3])
  ;;                                         'face '(:family "FontAwesome"))
  ;;                             :major)
  :bind
  (("C-c g" . magit-status)            ; Display the main magit popup
   ("C-c C-g" . magit-dispatch-popup)  ; Display keybinds for magit
   ("C-c C-l" . magit-log-buffer-file) ; Show log for the blob or file visited in the current buffer
   )
  :config
  (setq magit-diff-refine-hunk 'all)    ; Show fine differences for all displayed diff hunks
  (setq magit-log-section-commit-count 80) ; How many recent commits to show in certain log sections
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  )

(use-package git-gutter-fringe
  ;; Shows git additions/deletions/edits on the fringe
  :ensure
  :if window-system
  :bind (("C-x g g"   . git-gutter)
         ("C-x g n"   . git-gutter:next-hunk)
         ("C-x g p"   . git-gutter:previous-hunk)
         ("C-x g s"   . git-gutter:stage-hunk)
         ("C-x g v"   . git-gutter:revert-hunk)
         ("C-x g TAB" . git-gutter:popup-hunk)
         ("C-x g SPC" . git-gutter:mark-hunk))
  :config
  (global-git-gutter-mode t)
  (fringe-helper-define 'git-gutter-fr:modified nil
    "........"
    "...XX..."
    "..XXXX.."
    ".XXXXXX."
    ".XXXXXX."
    "..XXXX.."
    "...XX..."
    "........"))

(use-package magit-file-icons
  ;; Display icons for filenames in Magit buffers!
  :ensure
  :after magit
  :init
  (magit-file-icons-mode 1)
  :custom
  ;; These are the default values:
  (magit-file-icons-enable-diff-file-section-icons t)
  (magit-file-icons-enable-untracked-icons t)
  (magit-file-icons-enable-diffstat-icons t))

;;; ---------------------------------------------------------------------------

(use-package imenu-list
  ;; Update and show the imenu-list buffer.
  :ensure
  :defer
  :bind ("C-'" . imenu-list-smart-toggle)
  :config
  ;; (setq imenu-list-position 'left
  ;;       imenu-list-size 0.15
  ;;       imenu-list-auto-resize nil)
  )

;;; ---------------------------------------------------------------------------

(use-package ivy
  ;; Incremental Vertical completYon
  :ensure swiper
  :demand
  :bind (("C-c C-r" . ivy-resume)) ; resumes the latest ivy-based completion
  :custom
  (ivy-use-virtual-buffers t   "add recent files and bookmarks")
  (ivy-display-style 'fancy    "style for formatting the minibuffer")
  (ivy-height 16               "number of lines for the minibuffer window")
  (ivy-count-format "%d|%d "   "style to use for displaying the current candidate count")
  (ivy-wrap t                  "wrap around after the first and the last candidate")
  (ivy-extra-directories nil   "remove ../ and ./ from file name completion")
  (ivy-use-selectable-prompt t "make the prompt line selectable like a candidate")
  :config
  (ivy-mode 1))

;;;;;;;; avy ??????

(use-package swiper ; isearch with an overview
  :ensure
  :bind (([remap isearch-forward]  . swiper)
         ([remap isearch-backward] . swiper)
         ([f3]                     . swiper))
  :custom
  (swiper-action-recenter t "recenter display after exiting ‘swiper’"))

(use-package counsel ; various completion functions using Ivy
  :ensure
  :bind (;; ivy-based interface to standard commands
         ("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f"   . counsel-describe-function)
         ("C-h v"   . counsel-describe-variable)
         ;; ("<f1> l"  . counsel-find-library)
         ;; ("<f2> i"  . counsel-info-lookup-symbol)
         ;; ("<f2> u"  . counsel-unicode-char)
         ;; ivy-based interface to shell and system tools
         ;; ("C-c g"   . counsel-git)
         ;; ("C-c j"   . counsel-git-grep)
         ;; ("C-c k"   . counsel-ag)
         ;; ("C-x l"   . counsel-locate)
         ;; ("C-S-o"   . counsel-rhythmbox)
         ("C-c c t"   . counsel-load-theme)
         )
  :custom
  (counsel-find-file-at-point t "add file-at-point to the list of candidates")
  :config
  (counsel-mode))

;; (use-package ivy-rich
;;   ;; more friendly interface for ivy
;;   :ensure
;;   :after ivy
;;   :config
;;   (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)
;;   ;; align the virtual buffers
;;   (setq ivy-virtual-abbreviate 'full)
;;   (setq ivy-rich-switch-buffer-align-virtual-buffer t)
;;   (setq ivy-rich-path-style 'abbrev)
;;   )

;;; ---------------------------------------------------------------------------
;;; company-mode: Modular in-buffer completion framework for Emacs

;; Completion will start automatically after you type a few letters. Use
;; M-n and M-p to select, <return> to complete or <tab> to complete the
;; common part. Search through the completions with C-s, C-r and
;; C-o. Press M-(digit) to quickly complete with one of the first 10
;; candidates.

;; Type M-x company-complete to initiate completion manually. Bind this
;; command to a key combination of your choice.

;; When the completion candidates are shown, press <f1> to display the
;; documentation for the selected candidate, or C-w to see its
;; source. Not all back-ends support this.

(use-package company
  ;; Code completion framework. The name stands for “complete anything”
  :ensure
  ;;:delight (company-mode "C…")
  :bind ([(control .)] . company-complete)
  :hook (after-init . global-company-mode)
  :config
  (setq company-echo-delay 0.01)        ; default: 0.01 ; remove annoying blinking
  :custom
  (company-idle-delay 0.0)              ; idle delay in seconds until completion starts automatically; default: 0.2
  (company-minimum-prefix-length 3)     ; minimum prefix length for idle completion; default: 3
  (company-show-quick-access t)         ; show quick-access numbers; default: nil
  (company-tooltip-limit 20)            ; maximum number of candidates in the tooltip; default: 10
  (company-tooltip-align-annotations t) ; align annotations to the right tooltip border; default: nil

  ;; desired key bindings for: company-manual-begin
  ;; company-complete-common company-complete company-select-next
  )

;; (use-package company-box
;;   :ensure
;;   :hook (company-mode . company-box-mode))

;; (when (member "Symbola" (font-family-list))
;;   (message "Symbola font found.")
;;   ;; Missing Glyphs
;;   ;; If I ever use a font with a missing glyph, this will let Emacs check the
;;   ;; Symbola font (http://users.teilar.gr/~g1951d/) for the missing data.
;;   (set-fontset-font t 'symbol (font-spec :name "Symbola" :size 16) nil 'preppend))

(defun my/set-symbola-font (frame)
  "Adjusts the font settings of FRAME so Emacs can display emoji properly."
  (message "Looking for Symbola font...")
  (if (find-font (font-spec :name "Symbola"))
      (progn
        (message " found.")
        (set-fontset-font t 'symbol (font-spec :name "Symbola") frame 'preppend))
    (message " not found.")))

;; For when Emacs is started in GUI mode.
(my/set-symbola-font nil)

;; Hook for when a frame is created with emacsclient
;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
(add-hook 'after-make-frame-functions #'my/set-symbola-font)

;; (add-hook 'before-make-frame-hook (lambda () (message "BEFORE MAKE FRAME")))
;; (add-hook 'after-make-frame-functions (lambda (f) (message "AFTER MAKE FRAME")
;;                                         (message "Looking for Symbola font...")
;;                                         (if (find-font (font-spec :name "Symbola"))
;;                                             (progn
;;                                               (message "..............FOUND.")
;;                                               (set-fontset-font t 'symbol (font-spec :name "Symbola" :size 16) frame 'preppend))
;;                                           (message "...........NOT FOUND."))) )


(use-package company-emoji
  ;; company-mode backend providing autocompletion for emoji. :cool::sweat_drops:
  :ensure
  :after company
  :config (add-to-list 'company-backends 'company-emoji))

;; (use-package emoji-fontset
;;   :ensure t
;;   :if window-system
;;   :init
;;   (emoji-fontset-enable "Symbola"))

;;; ---------------------------------------------------------------------------

(use-package yasnippet
  ;; template system (Yet another snippet extension for Emacs)
  :ensure
  :defer
  ;; :delight (yas-minor-mode . "Ⓨ")
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all))

;;; ---------------------------------------------------------------------------

(use-package meson-mode
  ;; major mode for the Meson build system
  :ensure
  :defer
  :hook (meson-mode . company-mode))

(use-package flycheck
  ;; modern on-the-fly syntax checking
  :ensure
  :custom (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :hook (after-init . global-flycheck-mode))

;; (use-package flycheck-color-mode-line
;;   ;; colors the mode line according to the Flycheck status
;;   :ensure
;;   :after flycheck
;;   :hook (flycheck-mode . flycheck-color-mode-line-mode))

(use-package flycheck-pos-tip
  ;; shows flycheck error messages in a graphical popup
  :ensure
  :after flycheck
  :custom (flycheck-pos-tip-timeout 15)
  :config (flycheck-pos-tip-mode))

;; (use-package flycheck-inline
;;   ;; Display flycheck error message with inline popup style
;;   :ensure
;;   :after flycheck
;;   :hook (flycheck-mode . flycheck-inline-enable))

(use-package flycheck-status-emoji
  ;; adds cute emoji (e.g. 😱 for errors) to Flycheck’s mode line status
  :ensure
  :after flycheck
  :hook (flycheck-mode . flycheck-status-emoji-mode))

(use-package irony
  ;; A C/C++/Objective-C minor mode powered by libclang
  :ensure
  :disabled
  :defer000000
  ;; :delight "👔"
  
  :init
  (message "INIT irony...")
  (add-hook 'c-mode-hook #'irony-mode)
  (add-hook 'c++-mode-hook #'irony-mode)
  (add-hook 'objc-mode-hook #'irony-mode)
  
  :config
  (message "CONFIGURE irony...")
  (add-hook 'irony-mode-hook #'electric-pair-mode)
  (add-hook 'irony-mode-hook #'flycheck-mode)
  (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)
  (bind-keys :map irony-mode-map
             ([remap completion-at-point] . irony-completion-at-point-async)
             ([remap complete-symbol]     . irony-completion-at-point-async))
  )

(use-package company-irony
  :ensure
  :disabled
  :after (irony company)
  :hook (irony-mode . company-irony-setup-begin-commands)
  :config (add-to-list 'company-backends 'company-irony))

(use-package company-irony-c-headers
  ;; a company-mode backend for C/C++ header files that works with irony-mode
  :ensure
  :disabled
  :after company-irony
  :config (add-to-list 'company-backends 'company-irony-c-headers))

(use-package flycheck-irony
  :ensure
  :disabled
  :defer
  :after (irony flycheck)
  :hook (flycheck-mode . flycheck-irony-setup))

(use-package irony-eldoc
  :ensure
  :disabled
  :after irony
  :hook (irony-mode . irony-eldoc))

(use-package flycheck-clang-analyzer
  :ensure
  :disabled
  :defer
  :after flycheck-irony
  :config
  ;; automatically sets itself up as next checker after irony
  (message "CONFIG flycheck-clang-analyzer")
  (flycheck-clang-analyzer-setup))



(use-package cmake-mode
  ;; Major-mode for editing CMake sources
  :ensure
  :defer)

(use-package cmake-font-lock
  ;; Advanced, type aware, highlight support for CMake
  :ensure
  :hook (cmake-mode . cmake-font-lock-activate))

;; (use-package cmake-ide
;;   ;; IDE-like features for CMake projects
;;   :disabled
;;   :ensure
;;   :defer
;;   :bind (("<f9>" . cmake-ide-compile))
;;   :hook (c-mode-common . (lambda ()
;;                            (cmake-ide-setup)
;;                            (setq cmake-ide-build-pool-use-persistent-naming t)
;;                            (when (cmake-ide--locate-project-dir)
;;                              (setq cmake-ide-build-dir (concat (cmake-ide--locate-project-dir) "build_"))))))

;; (use-package cmake-project
;;   ;; Minor-mode integrating the CMake build process with the Emacs ecosystem
;;   :disabled ; seems to be unmaintained
;;   :ensure
;;   :defer
;;   :hook ((c-mode . maybe-cmake-project-hook)
;;          (c++-mode . maybe-cmake-project-hook))
;;   :preface
;;   (defun maybe-cmake-project-hook ()
;;     (when (file-exists-p "CMakeLists.txt")
;;       (cmake-project-mode))))

;;; ---------------------------------------------------------------------------
;;; jka-compr-hook

(use-package jka-cmpr-hook
  ;; Support for reading, writing, and loading compressed files.
  :config
  (add-to-list 'jka-compr-load-suffixes ".gnucash")
  ;; these are just the .gz settings in `jka-cmpr-hook`
  (add-to-list 'jka-compr-compression-info-list
               ["\\.gnucash\\'"
                "compressing"        "gzip"         ("-c" "-q")
                "uncompressing"      "gzip"         ("-c" "-q" "-d")
                t t "\037\213"
                zlib-decompress-region])
  (jka-compr-update)
  (auto-compression-mode t))

;;; ---------------------------------------------------------------------
;;; speedbar

(use-package speedbar
  :disabled ; REMOVE
  :if window-system
  :config
  ;; (set-face-attribute 'speedbar-button-face nil :height 100)
  (dolist (face (list 'speedbar-file-face
                      'speedbar-directory-face
                      'speedbar-tag-face
                      'speedbar-selected-face
                      'speedbar-highlight-face))
    (set-face-attribute face nil
                        :family "Helvetica LT Std"
                        :width 'condensed
                        :height 0.9
                        :weight 'light
                        :slant 'normal))
  ;; (speedbar-add-supported-extension ".hs")
  ;; (speedbar t)
  ;; (other-frame 0)

  (add-hook 'speedbar-mode-hook
            (lambda ()
              (interactive)
              (other-frame 0)))
  )

;;; ---------------------------------------------------------------------
;;; latex

(use-package tex-site
  :ensure auctex
  :defer
  :hook
  (LaTeX-mode . (lambda () (local-unset-key "\"")))
  :custom
  (LaTeX-item-indent 0)
  :config
  ;; (setq-default TeX-master nil) ; Query for master file.
  )

(use-package latex
  ;; :disabled ;;; ERROR
  :ensure auctex
  :defer
  :config
  (add-to-list 'LaTeX-font-list '(?\C-k "\\alert{" "}"))
  (add-to-list 'LaTeX-font-list '(?\C-v "\\verb|" "|"))
  (add-to-list 'LaTeX-font-list '(?\C-p "\\pyginline|" "|"))
  (add-to-list 'LaTeX-font-list '(?\C-m "\\mintinline{c++}{" "}"))
  ;; (add-to-list 'LaTeX-verbatim-macros-with-delims "pyginline")
  ;; (add-to-list 'LaTeX-verbatim-environments "pygmentex")
  )

;; REVIEW THIS JRM
(use-package auctex-latexmk
  ;; adds LatexMk support to AUCTeX
  :ensure
  :defer
  :init
  ;; M-x TeX-command-master (or C-c C-c) to use LatexMk command to compile TeX
  ;; source.
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  (auctex-latexmk-setup)
  :hook
  (TeX-mode . (lambda () (setq TeX-command-default "LatexMk")))
  (LaTeX-mode . (lambda () (setq TeX-command-default "LatexMk")))
  )

(use-package tex-item
  ;; Commands for working with tex items
  :ensure
  :disabled
  :after latex
  :config
  (defvar-keymap tex-item-map
    :repeat t
    "n" #'tex-item-forward
    "p" #'tex-item-backward
    "SPC" #'tex-item-mark
    "k" #'tex-item-kill
    "<backspace>" #'tex-item-backward-kill
    "t" #'tex-item-transpose
    "<down>" #'tex-item-move-down
    "<up>" #'tex-item-move-up)
  (define-key LaTeX-mode-map (kbd "M-g M-i") tex-item-map))

(use-package tex-parens
  ;; Like lisp.el but for tex
  :ensure
  :disabled
  :after latex
  :hook
  (tex-mode . tex-parens-mode)
  (TeX-mode . tex-parens-mode))

(use-package auctex-cont-latexmk
  ;; Run latexmk continuously, report errors via flymake
  :ensure
  :disabled
  :after latex
  :bind
  (:map LaTeX-mode-map
        ("C-c k" . auctex-cont-latexmk-toggle)))

(use-package preview-auto
  ;; Automatic previews in AUCTeX
  :ensure
  :disabled
  :after latex
  :config
  ;; Recommended AUCTeX settings
  (setq preview-locating-previews-message nil)
  (setq preview-protect-point t)
  (setq preview-leave-open-previews-visible t)
  :custom
  (preview-auto-interval 0.1)
  ;; Optimization: makes preview always use DVI’s, which generate faster than PDF’s:
  (preview-LaTeX-command-replacements '(preview-LaTeX-disable-pdfoutput))
  )

(use-package company-auctex
  :ensure
  :defer
  :after company
  :hook (TeX-mode-hook . company-auctex-init))

(use-package company-math
  :ensure
  :defer
  :after company
  :config
  (message "CONFIG company-math")
  ;; global activation of the unicode symbol completion
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  ;; local configuration for TeX modes
  (add-hook 'TeX-mode-hook
            (lambda ()
              (setq-local company-backends
                          (append '(company-math-symbols-latex company-latex-commands)
                                  company-backends)))))

(use-package cdlatex
  ;; Fast input methods for LaTeX environments and math
  :ensure
  :disabled
  :defer
  :after auctex
  :init
  (add-hook 'LaTeX-mode-hook #'turn-on-cdlatex)   ; with AUCTeX LaTeX mode
  (add-hook 'latex-mode-hook #'turn-on-cdlatex)   ; with Emacs latex mode
  )


;;; ---------------------------------------------------------------------
;;; cc-mode: C, C++, Objective-C, Java, CORBA's IDL, Pike, AWK

;; Folding
;;
;; C-c @ C-c 	hs-toggle-hiding
;; C-c @ C-h 	hs-hide-block
;; C-c @ C-l 	hs-hide-level
;; C-c @ C-s 	hs-show-block
;; C-c @ C-M-h 	hs-hide-all
;; C-c @ C-M-s 	hs-show-all
;;

(use-package hideshow
  ;; selectively display code/comment blocks
  :bind (:map hs-minor-mode-map ("C-c \\" . hs-toggle-hiding))
  :hook (prog-mode . hs-minor-mode))

;; Narrowing
;;
;; C-x n d 	narrow-to-defun
;; C-x n r 	narrow-to-region
;; C-x n n 	Narrow buffer to active region
;; C-x n w 	widen

(use-package cc-mode
  :defer
  :config
  ;;
  (setq-default c-basic-offset 2)
  
  ;; new key bindings
  (define-key c-mode-base-map [(control x) ?c] 'compile)
  
  ;; default styles of indentation:
  ;; user gnu k&r bsd stroustrup whitesmith ellemtel linux python java awk
  ;; C-c .		c-set-style
  ;; C-x h M-C-\	mark-whole-buffer indent-region
  (push '(c-mode . "ellemtel") c-default-style)
  (push '(c++-mode . "ellemtel") c-default-style)

  (c-add-style
   "e"
   '("gnu"
     (show-trailing-whitespace t)
     (indent-tabs-mode . nil)
     (tab-width . 8)
     (c-offsets-alist . ((defun-block-intro . 3)
                         (statement-block-intro . 3)
                         (case-label . 1)
                         (statement-case-intro . 3)
                         (inclass . 3)
                         ))))
  )

(use-package google-c-style
  :disabled
  :ensure
  :hook (((c-mode c++-mode) . google-set-c-style)
         (c-mode-common . google-make-newline-indent)))

(use-package modern-cpp-font-lock
  ;; Font-locking for "Modern C++"
  :ensure
  :after cc-mode
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package clang-format
  :when (executable-find "clang-format")
  :ensure
  :defer
  :bind ([C-c C-f] . clang-format-region)
  :custom (clang-format-style "llvm")
  )

(use-package eclim ;  an interface to the Eclipse IDE
  :disabled
  :ensure emacs-eclim
  :commands (eclim-mode)
  :config
  ;; (setq eclim-eclipse-dirs '("/opt/eclipse-jse-bin-4.5"))
  ;; (setq eclim-executable "/opt/eclipse-jse-bin-4.5/eclim")
  ;; (setq eclim-auto-save nil)
  (global-eclim-mode))

(use-package eclimd ; start and stop eclimd from within emacs
  :disabled
  :ensure emacs-eclim
  :commands (eclimd--running-p start-eclimd)
  :init
  (add-hook 'java-mode-hook
            (lambda ()
              ;; if eclimd is running, use it.
              (when (eclimd--running-p)
                (eclim-mode))))
  :config
  (setq eclimd-default-workspace "/alt/classes/cc1/2015-2/workspace"))

(use-package company-emacs-eclim
  :disabled
  :ensure emacs-eclim
  :after (eclim company)
  :config (company-emacs-eclim-setup))

;; (use-package meghanada ; Java IDE
;;   :ensure t
;;   :commands meghanada-mode
;;   :hook (java-mode-hook . meghanada-mode)
;;   ;; :config
;;   ;; (setq meghanada-use-company t
;;   ;;       meghanada-use-flycheck t
;;   ;;       meghanada-auto-start t)
;;   )

                                        ; lsp-mode with
                                        ;   with ccls
                                        ;     or clangd
                                        ; cquery

;;; ---------------------------------------------------------------------
;;; lsp client
;;; I will use eglot instead of lsp-mode

;; (use-package lsp-mode
;;   ;; client for the language server protocol
;;   :ensure
;;   :hook (
;;          (c-mode . lsp-deferred)
;;          (c++-mode . lsp-deferred)
;;          (objc-mode . lsp-deferred)
;;          (haskell-mode . lsp-deferred)
;;          (haskell-literate-mode-hook . lsp-deferred)
;;          (tuareg-mode . lsp-deferred)
;;          ;;
;;          (lsp-mode . lsp-enable-which-key-integration)
;;          )
;;   )

;; (use-package lsp-ui
;;   ;; UI integrations for lsp-mode: higher level UI modules of lsp-mode
;;   ;; (fancy sideline, popup documentation, VScode-like peek UI, etc.)
;;   :ensure
;;   :after lsp-mode
;;   ;; :commands lsp-ui-mode
;;   ;; :hook (lsp-mode . lsp-ui-mode)
;;   :custom
;;   (lsp-ui-sideline-delay 0.3 "Number of seconds to wait before showing sideline")
;;   )

;; (use-package lsp-ivy
;;   ;; interactive ivy interface to the workspace symbol functionality
;;   ;; offered by lsp-mode
;;   :ensure
;;   :after lsp-mode)

;; (use-package lsp-treemacs
;;   ;; Integration between lsp-mode and treemacs and implementation of
;;   ;; treeview controls using treemacs as a tree renderer.
;;   :ensure
;;   :after lsp-mode)

;; ;; (use-package ccls
;; ;;   :ensure
;; ;;   :defer)

(use-package eglot
  ;; Emacs Polyglot: Emacs client for the Language Server Protocol (LSP)
  :hook (
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (objc-mode . eglot-ensure)
         (haskell-mode . eglot-ensure)
         (haskell-literate-mode-hook . eglot-ensure)
         (tuareg-mode . eglot-ensure)
         (nix-mode . eglot-ensure)
         ;; (sh-mode . eglot-ensure)
         )
  :custom
  ;; (eglot-autoshutdown t)  ;; shutdown language server after closing last file
  (eglot-confirm-server-initiated-edits nil)  ;; allow edits without confirmation
  :bind (
         ("C-c h i" . eglot-find-implementation)
         ("C-c h e" . eglot)
         ("C-c h k" . eglot-shutdown-all)
         ("C-c h r" . eglot-rename)
         ("C-c h a" . eglot-code-actions)
         ("C-c h m" . eglot-menu)
         ("C-c h f" . eglot-format-buffer)
         )  
  :hook
  ;; https://emacs.stackexchange.com/questions/73983/how-to-make-eldoc-only-popup-on-demand
  (eglot-managed-mode . (lambda ()
                          ;; we want eglot to setup callbacks from eldoc, but we don't want eldoc
                          ;; running after every command. As a workaround, we disable it after we just
                          ;; enabled it. Now calling `M-x eldoc` will put the help we want in the eldoc
                          ;; buffer. Alternatively we could tell eglot to stay out of eldoc, and add
                          ;; the hooks manually, but that seems fragile to updates in eglot.
                          (eldoc-mode -1)))
  )

(use-package tree-sitter ;; ?????????????????????????????
  :ensure
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs ;; ?????????????????????????????
  :ensure
  :after tree-sitter)

;;; ---------------------------------------------------------------------
;;; python-mode

;; (setq py-shell-name "ipython")

;; jedi installation with package.el (Marmalade or MELPA)
;; requirements:
;;     dev-python/virtualenv
;; Type:
;;     M-x package-install RET jedi RET
;;     M-x jedi:install-server RET
;; Then open Python file.

;; (when (fboundp 'jedi:setup)
;;   (add-hook 'python-mode-hook 'jedi:setup)
;;   (setq jedi:complete-on-dot t))

;;; ---------------------------------------------------------------------

(use-package haskell-mode
  ;; a Haskell editing mode
  :ensure
  :defer
  :bind (:map haskell-mode-map
         ;; ("F8" . haskell-navigate-imports)
         ;; ("C-c m i s" . haskell-sort-imports)
         ;; ("C-c m i a" . haskell-align-imports)
         ("C-c C-c" . haskell-compile)
         ("C-c C-l" . haskell-process-load-file)
         :map haskell-cabal-mode-map
         ("C-c C-c" . haskell-compile)
         ;; Recommended Haskell Mode bindings, see
         ;; http://haskell.github.io/haskell-mode/manual/latest/Interactive-Haskell.html
         )
  :hook
  (haskell-mode . tree-sitter-hl-mode)
   ;;(
   ;; (haskell-mode . interactive-haskell-mode)
   ;; (haskell-mode . flycheck-mode)
   ;; (haskell-mode . (lambda ()
   ;;                   ;; completion support: in order to provide candidates for
   ;;                   ;; identifiers defined locally in let and where blocks combine
   ;;                   ;; completion candidates from completion-at-point function
   ;;                   ;; (company-capf backend) and dynamic abbrevs
   ;;                   (set (make-local-variable 'company-backends)
   ;;                        (append '((company-capf company-dabbrev-code))
   ;;                                company-backends)))))
  :custom
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-suggest-remove-import-lines t)
  (haskell-process-log t))

;; (use-package lsp-haskell
;;   ;; Interacting with a Haskell language server such as
;;   ;; haskell-language-server or ghcide using Microsoft's Language
;;   ;; Server Protocol.
;;   :ensure
;;   :disabled
;;   :defer
;;   :config
;;   (setq lsp-haskell-server-path "ghcide")
;;   (setq lsp-haskell-server-args '())
;;   )

;;; ---------------------------------------------------------------------

(use-package clean-mode
  :load-path "lib/clean-mode/"
  :mode (("\\.[di]cl$" . clean-mode)
         ("\\.prj$" . clean-project-mode))
  :config (add-hook 'clean-mode-hook (lambda () (setq tab-width 4))))

;;; ---------------------------------------------------------------------

(use-package rust-mode ; Rust major mode
  :ensure
  :defer)

;; (use-package flycheck-rust ; Flycheck setup for Rust
;;   :ensure
;;   :defer
;;   :after rust-mode
;;   :hook (flycheck-mode-hook . flycheck-rust-setup))

;; (use-package racer                      ; Completion and navigation for Rust
;;   :ensure
;;   :defer
;;   :init (add-hook 'rust-mode-hook #'racer-mode)
;;   :config
;;   (validate-setq racer-rust-src-path (getenv "RUST_SRC_PATH"))
;;   :delight (racer-mode . "ⓡ"))

;; (use-package cargo ; Control Cargo
;;   :ensure
;;   :bind (:map rust-mode-map ("<f5>" . cargo-process-build))
;;   :init (add-hook 'rust-mode-hook #'cargo-minor-mode)
;;   :delight cargo-minor-mode)

;; (use-package toml-mode ; Toml for Cargo files
;;   :ensure
;;   :defer)

;;; ---------------------------------------------------------------------

(use-package go-mode
  :ensure
  :defer)

;; (use-package company-go
;;   :ensure
;;   :after (company go-mode)
;;   :config (add-to-list 'company-backends 'company-go))

;; (use-package go-eldoc
;;   :ensure
;;   :after go-mode
;;   :hook (go-mode-hook . go-eldoc-setup))

;;; ---------------------------------------------------------------------

(use-package prolog
  :mode (("\\.pl$" . prolog-mode)
         ("\\.m$" . mercury-mode))
  :config (setq prolog-system 'swi))

;;; ---------------------------------------------------------------------
;;; CPerl mode

;;; cperl-mode is preferred to perl-mode
;;; "Brevity is the soul of wit" <foo at acm.org>
;(defalias 'perl-mode 'cperl-mode)

;;; ---------------------------------------------------------------------
;;; OCaml

(use-package tuareg
  :ensure
  :defer
  :mode (("\\.ml[ily]?$" . tuareg-mode)
         ("\\.topml$" . tuareg-mode))
  :hook
  ((tuareg-mode
    . (lambda ()
        ;; highlight trailing whitespace
        (setq show-trailing-whitespace t)

        ;; visually indicate empty lines after the buffer end
        (setq indicate-empty-lines t)

        ;; (setq tuareg-use-smie nil)
        ;; (setq tuareg-match-clause-indent 3)

        ;; ;; shell command used to compile ocaml programs
        ;; (unless (or (file-exists-p "makefile")
        ;;             (file-exists-p "Makefile"))
        ;;   (set (make-local-variable 'compile-command)
        ;;        (concat "ocamlbuild "
        ;;                (file-name-base buffer-file-name)
        ;;                ".native")))
        ))
   (tuareg-interactive-mode
    . (lambda ()
        (local-set-key [(control return)] 'comint-send-input))))

  :config
  (defun my/open-dune-file ()
    "Opens the nearest enclosing dune-file."
    (interactive)
    (find-file (string-join (list (locate-dominating-file buffer-file-name "dune") "/dune"))))
  (bind-key (kbd "C-c C-q") #'init-ocaml/open-dune-file tuareg-mode-map)
  )

(use-package utop
  :ensure
  :disabled
  :hook (tuareg-mode . utop-minor-mode))

(use-package ocp-indent
  ;; a simple tool and library to indent OCaml code
  :ensure
  :disabled
  :defer)

(use-package merlin
  :ensure
  :disabled
  :after company
  :hook ((tuareg-mode caml-mode) . merlin-mode)
  :config
  ;; make company aware of merlin
  (add-to-list 'company-backends 'merlin-company-backend))

;; Some useful key bindings:
;;   C-c C-x    merlin-next-error
;;   M-<tab>    completion-at-point
;;   C-c <tab>  merlin-try-completion
;;   C-c C-t    merlin-type-enclosing
;;     C-<up>
;;     C-<down>
;;   C-c t      merlin-type-expr
;;   C-c C-n    merlin-phrase-next
;;   C-c C-p    merlin-phrase-previous
;;   C-c C-l    merlin-locate
;;   C-c C-u    merlin-refresh
;;   C-c C-r    merlin-restart-process

(use-package flycheck-ocaml
  ;; OCaml support for Flycheck using Merlin
  :ensure
  :disabled
  :after (flycheck merlin)
  :config
  ;; Disable Merlin's own error checking
  (setq merlin-error-after-save nil)
  ;; Enable Flycheck checker
  (flycheck-ocaml-setup))

(use-package dune
  ;; Integration with the dune build system
  :ensure
  :defer)

(use-package dune-flymake
  ;; Flymake support for dune files
  ;; Provided by dune ?
  :after (:all dune flymake))

(use-package dune-format
  ;; Reformat OCaml's dune files automatically
  :ensure
  :bind (:map dune-mode-map ("C-c C-f" . dune-format)))

(use-package ocamlformat
  ;; :straight (:host github :repo "ocaml-ppx/ocamlformat" :files ("emacs/ocamlformat.el"))
  ;; :custom (ocamlformat-show-errors nil)
  :bind (:map tuareg-mode-map
              ("M-<iso-lefttab>" . ocamlformat)))

;; Add opam emacs directory to your load-path by appending this to your .emacs:
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)))

;;;----------------------------------------------------------------------------

(use-package scheme
  :defer
  :mode ("\\.sc" . scheme-mode)
  :config
  (add-hook
   'scheme-mode-hook
   (lambda ()
     (setq scheme-program-name "mzscheme")
     (put 'when 'scheme-indent-function 1)
     (put 'begin0 'scheme-indent-function 0)
     (put 'module 'scheme-indent-function 1)
     (put 'import 'scheme-indent-function 0)
     (put 'export 'scheme-indent-function 0)
     (put 'include 'scheme-indent-function 0)
     (turn-on-font-lock)
     (mapc (lambda (mode)
             (font-lock-add-keywords
              mode
              `((,(concat "("
                          (regexp-opt '("module"
                                        ) t)
                          "\\s-+\\(\\sw+\\)?")
                 (1 font-lock-keyword-face)
                 (2 font-lock-constant-face nil t))
                (,(concat "(\\(define-inline\\)\\>[ \t]*(?\\(\\sw+\\)?")
                 (1 font-lock-keyword-face)
                 (2 font-lock-function-name-face nil t))
                (,(concat "("
                          (regexp-opt '("match" "sxml-match") t)
                          "\\s-+\\(\\sw+\\)?")
                 (1 font-lock-keyword-face)
                 (2 font-lock-variable-name-face nil t))
                (,(concat "("
                          (regexp-opt '("in" "guard" "when" "unless" "fluid-let"
                                        "begin0"
                                        "use"
                                        "with"
                                        "library"
                                        "include"
                                        "import"
                                        "export") t)
                          "\\>")
                 (1 font-lock-keyword-face))
                ("(\\(error\\)\\>"
                 (1 font-lock-warning-face)))))
           '(scheme-mode inferior-scheme-mode))))  
  )

;;; ---------------------------------------------------------------------

(use-package folding
  :ensure
  :defer
  :commands (folding-mode turn-on-folding-mode turn-off-folding-mode folding-add-to-marks-list)
  :config
  (setq folding-mode-prefix-key (kbd "C-:"))
  (setq folding-folding-on-startup nil)
  (setq-default folding-internal-margins nil)
  ;; (folding-install)
  ;; (folding-install-hooks)
  ;; (add-hook 'after-revert-hook #'folding-mode-find-file t)
  )

(use-package fold-dwim
  :ensure
  :bind (([(control kp-4)] . fold-dwim-hide-all)
         ([(control kp-5)] . fold-dwim-toggle)
         ([(control kp-6)] . fold-dwim-show-all)))

(use-package lua-mode
  :ensure
  :hook
  ((lua-mode . folding-mode)
   (lua-mode . hs-minor-mode))
  :custom
  (lua-indent-level 2 "amount by which Lua subexpressions are indented")
  (lua-indent-nested-block-content-align nil "align the content of nested blocks with the open parenthesis")
  (lua-documentation-function 'eww)
  :config
  (folding-add-to-marks-list 'lua-mode "-- {{{" "-- }}}" nil t))

;;; ---------------------------------------------------------------------

(use-package web-mode
  ;; autonomous emacs major-mode for editing web templates http://web-mode.org/
  :ensure
  :mode (("\\.phtml\\'"     . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'"   . web-mode)
         ("\\.as[cp]x\\'"   . web-mode)
         ("\\.erb\\'"       . web-mode)
         ("\\.mustache\\'"  . web-mode)
         ("\\.djhtml\\'"    . web-mode)
         ("\\.html?\\'"     . web-mode))
  :config
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  ;; (add-to-list 'web-mode-ac-sources-alist '("html" . (ac-source-html-tag
  ;;                                                     ac-source-html-attribute
  ;;                                                     ac-source-html-attribute-2)))
  (defun my/web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-style-padding 2)
    (setq web-mode-script-padding 2)
    (setq web-mode-block-padding 2)
    (setq web-mode-comment-style 2)
    (setq web-mode-enable-css-colorization t)
    )
  (add-hook 'web-mode-hook  #'my/web-mode-hook)
  )

(use-package company-web-html
  ;; alternative autocompletion in html-mode, web-mode, jade-mode, slim-mode
  :ensure company-web
  :after (company web-mode)
  :config (add-to-list 'company-backends 'company-web-html))

;;; ---------------------------------------------------------------------------
;;; Mutt support

(use-package mutt-mode
  ;; major mode for editing mutt configuration
  :ensure
  :defer
  )

(use-package post
  ;; use emacs as an external editor for mail and news
  ;; https://github.com/zedinosaur/post-mode
  :load-path "cloned/post-mode/"
  :mode (
         ("mutt-[a-zA-Z0-9-.]+-[0-9]+-[0-9]+\\(-[a-fA-F0-9]+\\)?\\'" . post-mode)
         ( "mutt[a-zA-Z0-9._-]\\{6\\}\\'" . post-mode)
         ( "\\.\\(followup\\|letter\\|article\\)$" . post-mode)
         
         ( "\\.*mutt-*\\'" . post-mode)
         )
  :config
  (add-hook 'post-mode-hook (lambda ()
                              (auto-fill-mode t)
                              (setq fill-column 72)    ; rfc 1855 for usenet messages
                              (footmode-mode t))))

(use-package footnote
  ;; A mode to insert footnotes in texts and messages.
  :defer
  :init
  (add-hook 'text-mode-hook #'footnote-mode)
  )

(use-package boxquote
  ;; A handy trick for drawing a box around things. Good for quotes in emails and docs.
  :ensure
  :defer
  :config
  (setq-default boxquote-bottom-corner "╰"      ; U+2570
                boxquote-side          "│ "     ; U+2572 + space
                boxquote-top-and-tail  "────"   ; U+2500 (×4)
                boxquote-top-corner    "╭")     ; U+256F
  )

;; (setq-default fill-column 76)
;; (setq fill-individual-varying-indent t) ; ??????????????

;;; ---------------------------------------------------------------------------
;;; Themes

(use-package adwaita-dark-theme             :ensure :defer) ; dark
(use-package alect-themes                   :ensure :defer) ; dark light
(use-package apropospriate-theme            :ensure :defer) ; dark light
(use-package arc-dark-theme                 :load-path "cloned/arc-dark-theme" :defer) ; dark
;; (use-package avk-emacs-themes               :ensure :defer) ; dark light
;; (use-package ayu-theme                      :ensure :defer) ; dark light
;; (use-package berrys-theme                   :ensure :defer) ;      light
;; (use-package badwolf-theme                  :ensure :defer) ; dark
;; (use-package brutalist-theme                :ensure :defer) ;      light
;; (use-package challenger-deep-theme          :ensure :defer) ; dark
;; (use-package chyla-theme                    :ensure :defer) ;      light
;; (use-package cloud-theme                    :ensure :defer) ;      light
;; (use-package color-theme-sanityinc-tomorrow :ensure :defer) ; dark light
;; (use-package danneskjold-theme              :ensure :defer) ; dark
;; (use-package dark-mint-theme                :ensure :defer) ; dark
;; (use-package doneburn-theme                 :ensure :defer) ;      light
(use-package dream-theme                    :ensure :defer) ; dark
(use-package ef-themes                      :ensure :defer) ; dark light
;; (use-package eink-theme                     :ensure :defer) ;      light
;; (use-package exotica-theme                  :ensure :defer) ; dark
;; (use-package faff-theme                     :ensure :defer) ;      light
;; (use-package fantom-theme                   :ensure :defer) ; dark
;; (use-package flatfluc-theme                 :ensure :defer) ;      light
;; (use-package flatui-dark-theme              :ensure :defer) ; dark
;; (use-package flucui-themes                  :ensure :defer) ; dark light
;; (use-package forest-blue-theme              :ensure :defer) ; dark
;; (use-package github-modern-theme            :ensure :defer) ; light
(use-package gotham-theme            :ensure :defer) ; dark
;; (use-package goose-theme                    :ensure :defer) ; light
;; (use-package grayscale-theme                :ensure :defer) ; dark
;; (use-package gruvbox-theme                  :ensure :defer) ; dark
;; (use-package hamburg-theme                  :ensure :defer) ; dark
;; (use-package horizon-theme                  :ensure :defer) ; dark
;; (use-package humanoid-themes                :ensure :defer) ; dark light
;; (use-package idea-darkula-theme             :ensure :defer) ; dark
;; (use-package intellij-theme                 :ensure :defer) ;      light
(use-package iosevka-theme             :ensure :defer) ; only fonts
;; (use-package jbeans-theme                   :ensure :defer) ; dark
;; (use-package kaolin-themes                  :ensure :defer) ; dark light
;; (use-package kooten-theme                   :ensure :defer) ; dark
;; (use-package lab-themes                     :ensure :defer) ; dark light
;; (use-package labburn-theme                  :ensure :defer) ; dark
;; (use-package liso-theme                     :ensure :defer) ; dark
;; (use-package majapahit-theme                :ensure :defer) ; dark light
;; (use-package material-theme                 :ensure :defer) ; dark light
;; (use-package moe-theme                      :ensure :defer) ; dark light
;; (use-package molokai-theme                  :ensure :defer) ; dark
;; (use-package monokai-pro-theme              :ensure :defer) ; dark
;; (use-package mood-one-theme                 :ensure :defer) ; dark
;; (use-package naysayer-theme                 :ensure :defer) ; dark
;; (use-package nimbus-theme                   :ensure :defer) ; dark
(use-package nord-theme                     :ensure :defer) ; dark
(use-package nordic-night-theme             :ensure :defer) ; dark
;; (use-package nubox                          :ensure :defer) ; dark light tty
;; (use-package one-themes                     :ensure :defer) ; dark light
;; (use-package organic-green-theme            :ensure :defer) ;      light
;; (use-package panda-theme                    :ensure :defer) ; dark
;; (use-package parchment-theme                :ensure :defer) ;      light
;; (use-package planet-theme                   :ensure :defer) ; dark
;; (use-package purp-theme                     :ensure :defer) ; 
;; (use-package qtcreator-theme                :ensure :defer) ;      light
;; (use-package reykjavik-theme                :ensure :defer) ; dark
;; (use-package seoul256-theme                 :ensure :defer) ; dark light
;; (use-package silkworm-theme                 :ensure :defer) ;      light
;; (use-package snazzy-theme                   :ensure :defer) ; dark
;; (use-package srcery-theme                   :ensure :defer) ; dark
(use-package standard-themes                   :ensure :defer) ; dark light
;; (use-package sublime-themes                 :ensure :defer)
;; (use-package sunburn-theme                  :ensure :defer) ; dark
;; (use-package suscolors-theme                :ensure :defer) ; dark
;; (use-package tao-theme                      :ensure :defer) ; 
;; (use-package toxi-theme                     :ensure :defer) ; dark
;; (use-package tron-legacy-theme              :ensure :defer) ; dark
;; (use-package vs-dark-theme                  :ensure :defer) ; dark
;; (use-package vs-light-theme                 :ensure :defer) ;      light
;; (use-package vscdark-theme                  :ensure :defer) ; dark
;; (use-package vscode-dark-plus-theme         :ensure :defer) ; dark
;; (use-package yoshi-theme                    :ensure :defer) ; dark
;; (use-package zeno-theme                     :ensure :defer) ; dark
;; (use-package zerodark-theme                 :ensure :defer) ; dark

;; (use-package doom-themes
;;   :ensure
;;   :defer
;;   :config
;;   (doom-themes-visual-bell-config) ; enable flashing the mode-line on error
;;   (doom-themes-neotree-config) ; enable custom neotree theme (all-the-icons fonts must be installed!)
;;   (doom-themes-org-config) ; corrects (and improves) org-mode's native fontification
;;   )

(use-package modus-themes
  :ensure

  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t
        )

  :config
  ;; Load the theme of your choice.
  ;; (load-theme 'modus-operandi :no-confim)

  ;; :bind ("<f5>" . modus-themes-toggle)
  )

;; ;; http://stackoverflow.com/questions/9900232/changing-color-themes-emacs-24-order-matters/15595000#15595000
;; ;; (defadvice load-theme
;; ;;   (before theme-dont-propagate activate)
;; ;;   (mapc #'disable-theme custom-enabled-themes))

(use-package theme-looper
  :ensure
  :after ivy
  :bind (("C-}" . theme-looper-enable-next-theme)
         ("C-{" . theme-looper-enable-previous-theme)
         ("C-\\" . theme-looper-enable-random-theme)
         ("C-|" . theme-looper-select-theme)
         ("C-M-|" . theme-looper-select-theme-from-all))
  :config
  ;; (theme-looper-set-favorite-themes '(molokai distinguished *default*))
  ;; (theme-looper-set-ignored-themes '(cobalt))
  ;; (theme-looper-set-favorite-themes-regexp
  ;;  (string-join
  ;;   '("gruvbox-" "molokai" "distinguished" "tomorrow" "doom" "*default*")
  ;;   "\\|"))
  ;; (theme-looper-set-ignored-themes-regexp "solarized-gruvbox")
  ;; (theme-looper-enable-random-theme)
  )

(use-package remember-last-theme
  :ensure
  :config
  (remember-last-theme-with-file-enable
   (expand-file-name "last-theme.el" user-emacs-directory)))

(use-package theme-anchor
  ;; Apply a custom theme to current Emacs buffer only 
  :ensure
  :defer
  :commands (theme-anchor-buffer-local))

;; (set-face-attribute font-lock-comment-face nil :slant 'italic)
(set-face-attribute font-lock-comment-face nil :slant 'oblique)

;;; ---------------------------------------------------------------------------
;;; festival
;;;

;; (add-to-list 'load-path "/usr/share/festival")
;; (add-to-list 'load-path "/usr/lib/festival")
;; (autoload 'say-minor-mode "festival" "Menu for using Festival." t)
;; (say-minor-mode t)
;; (setq auto-mode-alist
;;       (append '(("\\.festivalrc$" . scheme-mode)) auto-mode-alist))

;;; ---------------------------------------------------------------------------
;;; ispell
;;;

(use-package ispell
  :defer
  :custom
  (ispell-silently-savep t)
  ;; (ispell-dictionary "pt_BR")
  ;; (ispell-program-name "hunspell")
  ;; (ispell-extra-args '("-i" "utf-8"))
  )

; Resets the dictionary list for the hunspell dictionaries.
(setq ispell-dictionary-alist
      '(
        (nil     "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-d" "en_US") nil utf-8)
        ("en_US" "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-d" "en_US") nil utf-8)
        ("pt_BR" "[a-zàáâãçéêíóôõúüA-ZÀÁÂÃÇÉÊÍÓÔÕÚÜ]" "[^a-zàáâãçéêíóôõúüA-ZÀÁÂÃÇÉÊÍÓÔÕÚÜ]" "" nil ("-d" "pt_BR") nil utf-8)
        ))

;;; ---------------------------------------------------------------------------
;;; emacs-tiny-tools
;;;
;;; http://www.emacswiki.org/emacs/TinyTools
;;; remember to git pull regularly
;;;

(let ((tiny-tools-lisp-dir
       (expand-file-name "lisp"
                         (expand-file-name "tiny-tools"
                                           (expand-file-name "lib" user-emacs-directory)))))
  (dolist (subdir '("tiny" "other"))
    (let ((dir (expand-file-name subdir tiny-tools-lisp-dir)))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))

(autoload 'turn-on-tinyprocmail-mode  "tinyprocmail" "" t)
(autoload 'turn-off-tinyprocmail-mode "tinyprocmail" "" t)
(autoload 'tinyprocmail-mode          "tinyprocmail" "" t)

(add-hook 'tinyprocmail--load-hook 'tinyprocmail-install)

;; Procmail files usually end to suffix "*.rc", like pm-file.rc
;; Some older procmail files start with "rc.*", like rc.file-name
(add-to-list
 'auto-mode-alist
 ;; '("\\.procmailrc\\|pm-.*\\.rc$\\|\\<rc\\.[a-z]" . turn-on-tinyprocmail-mode)
 '("\\.procmailrc\\|pm-.*\\.rc$" . turn-on-tinyprocmail-mode)
 )

;;; ---------------------------------------------------------------------------
;;; fvwm-mode
;;;

(use-package fvwm-mode
  ;; A major mode for editing Fvwm configuration files
  :ensure
  :defer)

;;;----------------------------------------------------------------------------
;;; ERC (Emacs IRC Client)
;;; To load ERC in Emacs: M-x erc-select
;;;

;;; ;; This will add it to the Tools menu. It uses EasyMenu.

;;; (require 'easymenu)
;;; (easy-menu-add-item  nil '("tools")
;;;   ["IRC" erc-select t])

;;; ;; keep the prompt line at the bottom of the window
;;; (add-hook 'erc-mode-hook 'erc-add-scroll-to-bottom)

;;; ;; To start ERC, use M-x erc-select. This will prompt you for server,
;;; ;; port, nick, and password, and it will use the values of the variables
;;; ;; below as defaults.

;;; (setq erc-server "irc.freenode.net"
;;;       erc-port 6667
;;;       erc-nick "malaquias"
;;;       erc-user-full-name "José Romildo Malaquias"
;;;       erc-email-userid "userid"    ; for when ident is not activated
;;;       erc-prompt-for-password nil) ; OPN doesn't require passwords

;;; (add-hook 'erc-mode-hook
;;;           (lambda ()
;;; 	     (require 'erc-autojoin)
;;; 	     (erc-autojoin-mode 1)
;;; 	     (setq erc-autojoin-channels-alist
;;; 		   '(("freenode.net" "#emacs" "#latex" "#icewm" "#mplayer" "#gentoo" "#gentoo-fr" "#gentoo-de" "#gentoo-pt")
;;; 		     ("ircnet.demon.co.uk" "#transcode")
;;; 		     ("irc.gnome.org" "#gnucash")))
;;; 	     (erc "irc.freenode.net" 6667 "malaca" "Romildo" nil)
;;; 	     (erc "ircnet.demon.co.uk" 6667 "malaca" "Romildo" nil)
;;; 	     (erc "irc.gnome.org" 6667 "malaca" "Romildo" nil)
;;; 	     ))

;;; ;; logging:
;;; (setq erc-log-channels t)
;;; (setq erc-log-channels-directory "~/.irc/logs/")
;;; (setq erc-save-buffer-on-part t)
;;; (setq erc-hide-timestamps nil)
;;; (setq erc-log-insert-log-on-open nil)

;;; (defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
;;;   (save-some-buffers t (lambda () (when (and (eq major-mode 'erc-mode)
;;;                                              (not (null buffer-file-name)))))))

;;; (add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
;;; (add-hook 'erc-mode-hook (lambda () (when (not (featurep 'xemacs))
;;;                                       (set (make-variable-buffer-local
;;;                                             'coding-system-for-write)
;;;                                            'emacs-mule))))
;;; ;; end logging


;;; ;; The following function and keybinding allow you to switch to the next
;;; ;; ERC buffer using `C-c e'. If no ERC buffers exist, or ERC has not
;;; ;; even been loaded yet, then `erc-select' is called to start a new
;;; ;; connection.

;;; (defun switch-to-irc ()
;;;   "Switch to an IRC buffer, or run `erc-select'.
;;;     When called repeatedly, cycle through the buffers."
;;;   (interactive)
;;;   (let ((buffers (and (fboundp 'erc-buffer-list)
;;; 		      (erc-buffer-list))))
;;;     (when (eq (current-buffer) (car buffers))
;;;       (bury-buffer)
;;;       (setq buffers (cdr buffers)))
;;;     (if buffers
;;; 	(switch-to-buffer (car buffers))
;;;       (erc-select))))

;;; (global-set-key (kbd "C-c e") 'switch-to-irc)

;;;----------------------------------------------------------------------------
;;; web browsers

(use-package browse-url
  ;; pass a URL to a WWW browser (part of GNU Emacs)
  :bind (("C-c u" . browse-url)))

(use-package w3m
  ;; A simple interface for w3m, a pager with WWW capability
  :ensure
  :defer
  :config
  ;; (setq w3m-pop-up-frames t)
  ;; (setq w3m-popup-frame-parameters
  ;;       '((font . "-*-trebuchet ms-medium-r-normal-*-*-160-*-*-*-*-*-*")
  ;;         (width . 60)))
  
  ;; (setq w3m-default-display-inline-images t)
  (setq w3m-use-cookies t)
  
  ;; (setq w3m-mode-map (make-sparse-keymap))
  (define-key w3m-mode-map [return      ] 'w3m-view-this-url)
  (define-key w3m-mode-map [?q          ] 'bury-buffer)
  (define-key w3m-mode-map [mouse-1     ] 'w3m-maybe-url)
  (define-key w3m-mode-map [f5          ] 'w3m-reload-this-page)
  (define-key w3m-mode-map [(meta left) ] 'w3m-view-previous-page)
  (define-key w3m-mode-map [(meta right)] 'w3m-view-next-page)
  
  (defun w3m-maybe-url ()
    (interactive)
    (if (or (equal '(w3m-anchor) (get-text-property (point) 'face))
            (equal '(w3m-arrived-anchor) (get-text-property (point) 'face)))
        (w3m-view-this-url)))
  
  )

;;; ---------------------------------------------------------------------------
;;; diff-mode

(add-hook 'diff-mode-hook
          (lambda ()
            (diff-auto-refine-mode 1)))


;;; ---------------------------------------------------------------------------
;;; ediff-mode

;; http://stackoverflow.com/questions/7038146/how-can-i-compare-two-files-similar-to-vims-vimdiff
(setq ediff-split-window-function 'split-window-horizontally)

;;;----------------------------------------------------------------------------
;;; smb-mode

(autoload 'smb-mode "smb-mode" "SMB Major Mode" t)
(add-to-list 'auto-mode-alist '("smb\\.conf$" . smb-mode))

;;;----------------------------------------------------------------------------
;;; apt-sources-mode

(use-package apt-sources-list
  ;; Mode for editing APT source.list files
  :ensure
  :defer)

;;;----------------------------------------------------------------------------
;;; ebuild-mode

;; (defun ebuild-mode ()
;;   (shell-script-mode)
;;   (sh-set-shell "bash")
;;   (make-local-variable 'tab-width)
;;   (setq tab-width 4))
;; (setq auto-mode-alist (cons '("\\.ebuild\\'" . ebuild-mode) auto-mode-alist))
;; (setq auto-mode-alist (cons '("\\.eclass\\'" . ebuild-mode) auto-mode-alist))

;;;----------------------------------------------------------------------------

(use-package nix-mode
  :ensure
  :defer
  :hook
  (nix-mode . (lambda ()
                (use-local-map nix-mode-map)
                (modify-syntax-entry ?- "_") ; hiphen should be a symbol constituent
                ;; (smartscan-mode 1) ; ????????
                )))

(use-package company-nixos-options
  :ensure
  :after (company nix-mode)
  :hook (nix-mode . (lambda ()
                      (add-to-list 'company-backends 'company-nixos-options))))

(use-package nix-update
  ;; Update "fetch" blocks in .nix expressions
  :ensure
  :after nix-mode)

(use-package nixpkgs-fmt
  ;; Reformat Nix code with nixpkgs-fmt
  :ensure
  :after nix-mode
  :config
  (message "nixpkgs-fmt ------------------------------------------")
  ;; :hook (nix-mode . nixpkgs-fmt-on-save-mode)
  ;; :bind (:map nix-mode-map ("C-c C-f" . nixpkgs-fmt)) ; ????????
  )

;;;----------------------------------------------------------------------------
;;; tabs

(use-package svg-tag-mode
  ;; A minor mode for Emacs that replace keywords with nice SVG labels
  :ensure nil
  :disabled
  :commands svg-tag-mode
  :hook (prog-mode)
  :config (setq svg-tag-tags '(("TODO" . ((lambda (tag) (svg-tag-make "TODO"))))))
  )

;;; tab-line-mode
(use-package emacs
  :config
  ;; (setq tab-line-separator " ")  ;; set it to empty
  (setq tab-line-separator (propertize "|" 'face  '(foreground-color . "cyan")))
  (global-tab-line-mode t))


;; (tab-bar-mode 1)
;; (setq tab-bar-show 1)                      ;; hide bar if <= 1 tabs open
;; (setq tab-bar-close-button-show nil)       ;; hide tab close / X button
;; (setq tab-bar-new-tab-choice "*dashboard*");; buffer to show in new tabs
;; (setq tab-bar-tab-hints t)                 ;; show tab numbers
;; (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))) ;; elements to include in bar

;; (when (require 'tabbar nil 'noerror)
;;   (tabbar-mode)

;;   ;; http://code.google.com/p/my-emacs-config/source/browse/trunk/home/.emacs-config/settings/tabbar-settings.el?r=93
;;   ;;;;(require 'tabbar-settings nil 'noerror)


;;   ;; (define-key global-map [(alt j)] 'tabbar-backward)
;;   ;; (define-key global-map [(alt k)] 'tabbar-forward)
;;   )

;; (setq tabbar-ruler-global-tabbar 't) ; If you want tabbar
;; ;; (setq tabbar-ruler-global-ruler 't)  ; if you want a global ruler
;; ;; (setq tabbar-ruler-popup-menu 't)    ; If you want a popup menu.
;; ;; (setq tabbar-ruler-popup-toolbar 't) ; If you want a popup toolbar
;; (when (require 'tabbar-ruler nil 'noerror)
;;   (set-face-attribute 'tabbar-default nil :family "sans" :width 'condensed :height 80)
;;   )

;; (use-package tabbar
;;   :disabled nil
;;   :ensure t
;;   ;; :bind (("<M-left>" . tabbar-backward-tab)
;;   ;;        ("<M-right>" . tabbar-forward-tab))
;;   ;; :demand
;;   :config  
;;   ;; (defun my/tabbar-buffer-groups ()
;;   ;;   "Return the list of group names the current buffer belongs to."
;;   ;;   (list
;;   ;;    (cond
;;   ;;     ((or (get-buffer-process (current-buffer))
;;   ;;          (tabbar-buffer-mode-derived-p major-mode '(comint-mode compilation-mode)))
;;   ;;      "Process")
;;   ;;     ((member (buffer-name) '("*scratch*" "*Messages*" "*Help*"))
;;   ;;      "Common")
;;   ;;     ((eq major-mode 'dired-mode)
;;   ;;      "Dired")
;;   ;;     ((memq major-mode '(help-mode apropos-mode Info-mode Man-mode))
;;   ;;      "Help")
;;   ;;     ((memq major-mode
;;   ;;            '(rmail-mode
;;   ;;              rmail-edit-mode vm-summary-mode vm-mode mail-mode
;;   ;;              mh-letter-mode mh-show-mode mh-folder-mode
;;   ;;              gnus-summary-mode message-mode gnus-group-mode
;;   ;;              gnus-article-mode score-mode gnus-browse-killed-mode))
;;   ;;      "Mail")
;;   ;;     ((and (stringp mode-name)
;;   ;;           ;; Take care of preserving the match-data because this
;;   ;;           ;; function is called when updating the header line.
;;   ;;           (save-match-data (string-match "[^ ]" mode-name)))
;;   ;;      mode-name)
;;   ;;     (t (symbol-name major-mode))
;;   ;;     )))

;;   ;; (defun my/tabbar-buffer-groups ()
;;   ;;   "Return the list of group names the current buffer belongs to."
;;   ;;   (list
;;   ;;    (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
;;   ;;          ((eq major-mode 'dired-mode) "emacs")
;;   ;;          (t "user"))))

;;   (defun my/tabbar-buffer-groups ()
;;     "Return the list of group names the current buffer belongs to."
;;     (list "all"))

;;   (setq tabbar-buffer-groups-function #'my/tabbar-buffer-groups)

;;   (defun my/tabbar-theme ()
;;     "Customize tabbar faces based on current theme."
;;     (require 'color) ; color manipulation library
;;     ;; (let ((base "wheat"))
;;     (let ((base (face-background 'mode-line))
;;           (fg (face-foreground 'mode-line)))
;;       (set-face-attribute 'tabbar-default nil
;;                           ;; :inherit 'variable-pitch
;;                           :inherit 'mode-line
;;                           :background base
;;                           :foreground fg
;;                           :family "Helvetica LT Std"
;;                           :width 'condensed)
;;       (set-face-attribute 'tabbar-unselected nil
;;                           :inherit 'tabbar-default
;;                           :box `(:line-width 2 :color ,base))
;;       (set-face-attribute 'tabbar-selected nil
;;                           :inherit 'tabbar-default
;;                           :background (color-darken-name base 20)
;;                           :weight 'bold
;;                           :box `(:line-width 2 :color ,(color-darken-name base 20)))
;;       (set-face-attribute 'tabbar-highlight nil ; tab during mouse-overs
;;                           :inherit 'tabbar-default
;;                           :underline t)
;;       (set-face-attribute 'tabbar-modified nil ; unsaved tabs
;;                           :inherit 'tabbar-default
;;                           :foreground "dark breen"
;;                           :strike-through t)
;;       (set-face-attribute 'tabbar-button nil
;;                           :inherit 'tabbar-default
;;                           :box `(:line-width 2 :color ,base))
;;       (set-face-attribute 'tabbar-button-highlight nil ; button during mouse-overs
;;                           :inherit 'tabbar-default
;;                           :box `(:line-width 2 :color ,(color-darken-name base 20)))
;;       (set-face-attribute 'tabbar-separator nil
;;                           :inherit 'tabbar-default
;;                           :height 0.7)
;;       ))

;;   ;; (advice-add 'load-theme :after
;;   ;;             (lambda (theme &optional no-confirm no-enable)
;;   ;;               (my/tabbar-theme)))

;;   (my/tabbar-theme)
  
;;   ;; Change padding of the tabs
;;   ;; we also need to set separator to avoid overlapping tabs by highlighted tabs
;;   (setq tabbar-separator '(1.0))

;;   (tabbar-mode t))


(use-package tabbar-ruler
  :ensure
  :disabled
  :bind ("C-c t" . tabbar-ruler-move)
  :init
  (setq tabbar-ruler-global-tabbar t)    ; get tabbar
  ;; (setq tabbar-ruler-global-ruler t)     ; get global ruler
  ;; (setq tabbar-ruler-popup-menu t)       ; get popup menu.
  ;; (setq tabbar-ruler-popup-toolbar t)    ; get popup toolbar
  ;; (setq tabbar-ruler-popup-scrollbar t)  ; show scroll-bar on mouse-move
  :config
  (tabbar-install-faces)
  )

(use-package centaur-tabs
  :ensure
  :disabled
  :demand
  :custom
  (centaur-tabs-style "bar")
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

;;; ---------------------------------------------------------------------------

(use-package ebs
  ;; easy buffer switch
  ;; obtained from http://www.emacswiki.org/elisp/ebs.el
  :bind ([(control tab)] . ebs-switch-buffer)
  :config
  (ebs-initialize))

(use-package webpaste
  ;; paste whole buffers or parts of buffers to several pastebin-like services
  :ensure
  :bind (("C-c C-p C-b" . webpaste-paste-buffer)
         ("C-c C-p C-r" . webpaste-paste-region))
  :config
  ;; (setq webpaste-provider-priority '("gist.github.com" "ptpb.pw" "dpaste.de"))
  ;; (setq webpaste/paste-confirmation t)
  ;; (setq webpaste/open-in-browser t)
  )

;;; ---------------------------------------------------------------------------
;;; shell-mode

;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;; ---------------------------------------------------------------------------
;;; matlab-mode

(use-package matlab
  :ensure matlab-mode
  :mode (("\\.m$" . matlab-mode)
         ("\\.sci$" . matlab-mode))
  :commands (matlab-shell)
  :config
  (setq matlab-indent-function t)	; if you want function bodies indented
  (setq matlab-verify-on-save-flag nil) ; turn off auto-verify on save
  (defun my/matlab-mode-hook ()
    (setq fill-column 76))		; where auto-fill should wrap
  (add-hook 'matlab-mode-hook 'my/matlab-mode-hook)
  (defun my/matlab-shell-mode-hook ()
    '())
  (add-hook 'matlab-shell-mode-hook 'my/matlab-shell-mode-hook)
  )

;; http://forge.scilab.org/index.php/p/scilab-emacs/source/tree/master/scilab.el
;;(load "scilab-startup")

;;; ---------------------------------------------------------------------------

(add-to-list 'special-display-buffer-names
             '("*Buffer List*"
               (modeline . nil)
               (unsplittable . nil)
               (top-toolbar-visible-p . nil)
               (menubar-visible-p . nil)
               (height . 10)
               (width . 70)
               (top . 0)
               (left . 444)
               (auto-raise-mode . t)
               ))

;; (define-key Buffer-menu-mode-map [return] 'Buffer-menu-other-window)

(use-package ibuffer
  :disabled
  :bind ([(control x) (control b)] . ibuffer-list-buffers)
  :init
  (add-to-list 'special-display-buffer-names "*Ibuffer*")
  :config
  ;; (define-key ibuffer-mode-map [return] 'ibuffer-visit-buffer-other-window)
  (setq ibuffer-shrink-to-minimum-size t)
  (setq ibuffer-default-sorting-mode 'major-mode))

;;; ---------------------------------------------------------------------------
;;; flex

(use-package flex-mode ; major mode for editing flex specifications
  :mode "\\.f?lex$")

(use-package bison-mode ; major mode for Bison, Yacc and Lex grammars
  :ensure
  :defer)

;;; ---------------------------------------------------------------------------
;;; tiger

(use-package tiger-mode ; major mode for editing Tiger programs
  :mode "\\.tig$")

;;; ---------------------------------------------------------------------------
;;; powerline

(use-package powerline
  ;; https://github.com/milkypostman/powerline
  ;; https://powerline.readthedocs.org/en/latest/installation.html
  :ensure nil
  :disabled
  :config
  (powerline-default-theme)
  ;; (powerline-center-theme)
  ;; (power-line-nano-theme)
  )

;; (require 'main-line nil 'noerror)

;;; ---------------------------------------------------------------------------
;;; visual-regex

(use-package visual-regexp
  :ensure t
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)))

;;;----------------------------------------------------------------------------

(use-package google-translate
  ;; Emacs interface to Google Translate
  :ensure
  :bind ("\C-c t" . google-translate-smooth-translate)

  :init
  (use-package popup :ensure :defer)

  (require 'google-translate-smooth-ui)

  (setq google-translate-translation-directions-alist
        '(("en" . "pt") ("pt" . "en")
          ("fr" . "pt") ("pt" . "fr")
          ("en" . "fr") ("fr" . "en")))

  (defun google-translate--search-tkk ()
    "Search TKK."
    (list 430675 2721866130))

  :custom
  ;; nil echo-area popup kill-ring current-buffer help
  ;; paragraph-overlay paragraph-insert
  ;; (google-translate-output-destination 'nil)
  (google-translate-pop-up-buffer-set-focus t)
  (google-translate-show-phonetic t)
  (google-translate-listen-program "mpv")
  ;; (google-translate-enable-ido-completion t)
  (google-translate-default-source-language "auto")
  (google-translate-default-target-language "pt")
  )

;;;----------------------------------------------------------------------------

(use-package reverso
  :ensure
  :defer)

(use-package org
  ;; keeping notes, maintaining TODO lists, planning projects, and authoring documents
  :defer
  :bind
  (("\C-c l" . org-store-link)
   ("\C-c a" . org-agenda)
   ("\C-c b" . org-iswitchb)
   ;; ("\C-c t" . orgtbl-mode)
   )
  :hook
  ((org-mode . turn-on-font-lock)
   (latex-mode . turn-on-orgtbl)))

(use-package epresent
  ;; Simple presentation mode for Emacs Org-mode
  :ensure
  :defer)

(use-package org-tree-slide
  ;; A presentation tool for org-mode based on the visibility of outline trees 
  :ensure
  :defer
  :config
  (define-key org-mode-map (kbd "<f8>") 'org-tree-slide-mode)
  (define-key org-mode-map (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle)
  (org-tree-slide-simple-profile)
  )

;;;----------------------------------------------------------------------------

(use-package conf-mode
  ;; mode for Unix and Windows Conf files and Java properties
  :mode (("\.tint2rc$" . conf-mode)
         ("\.xsettingsd$" . conf-mode)))

;;; ---------------------------------------------------------------------------

(use-package adoc-mode
  :ensure
  :mode ("\\.adoc\\'" "\\.asciidoc\\'")
  :hook
  (adoc-mode . visual-line-mode)
  ;; (adoc-mode . variable-pitch-mode)
  )

(use-package dokuwiki-mode
  ;; For smart org-tables in dokuwiki mode. Use M-x orgtbl-insert-radio-table
  :ensure
  :mode ("\\.dokuwiki\\'" . dokuwiki-mode)
  :config
  (defun orgtbl-to-dokuwiki (table params)
    "Convert the orgtbl-mode TABLE to DokuWiki."
    (orgtbl-to-generic
     table
     (org-combine-plists
      '(
        :lstart "|"
                :sep "|"
                :hlstart "^"
        )
      params)))
  (add-hook 'dokuwiki-mode-hook #'turn-on-orgtbl)
  )

(use-package pandoc-mode
  :ensure
  :hook (markdown-mode . pandoc-mode))

(use-package markdown-mode
  :ensure
  :commands (markdown-mode gfm-mode)
  :mode ("\\.md\\'" . gfm-mode)
  :custom
  (markdown-command "pandoc -t html5 --pdf-engine=weasyprint")
  (markdown-fontify-code-blocks-natively t)
  ;; :hook
  ;; (markdown-mode . turn-on-orgtbl)
  :config
  (advice-add 'markdown-preview :around
              (lambda (orig &rest args)
                "Use Firefox as default browser."
                (let ((browse-url-browser-function #'browse-url-firefox))
                  (apply orig args))))
  )

(use-package markdown-xwidget
  :disabled ; xwidget is not working for me; wait appearing in MELPA
  :load-path "cloned/markdown-xwidget/"
  :after markdown-mode
  ;; :straight (markdown-xwidget
  ;;            :type git
  ;;            :host github
  ;;            :repo "cfclrk/markdown-xwidget"
  ;;            :files (:defaults "resources"))
  :bind (:map markdown-mode-command-map
              ("x" . markdown-xwidget-preview-mode))
  :custom
  (markdown-xwidget-command "pandoc")
  (markdown-xwidget-github-theme "light")
  (markdown-xwidget-mermaid-theme "default")
  (markdown-xwidget-code-block-theme "default"))

(use-package edit-indirect
  ;; Edit regions in separate buffers. Markdown relies on this package
  ;; for to edit source code blocks like org mode.
  :ensure
  ;; :after expand-region ; to use region-prefix-map
  ;; :bind (:map region-prefix-map
  ;;             ("r" . edit-indirect-region))
  :defer)

;; (use-package poly-markdown
;;   :ensure)

;; (use-package poly-noweb
;;   :ensure)

;; (use-package poly-org
;;   :ensure)

;;; ---------------------------------------------------------------------------

(defun xfd (&optional xlfd)
  "Run the xfd program.
XLFD defaults to the selected frame's font, or the default face's font."
  (interactive (list (read-string "Font: "
                                  (cons (or (frame-parameter
					     (selected-frame)
					     'font)
                                            (face-font 'default))
                                        1))))
  (start-process "xfd" "*Messages*" "xfd"
                 "-fn" (or xlfd
                           (frame-parameter (selected-frame) 'font)
                           (face-font 'default))))

(use-package asy-mode
  ;; mode for editing Asymptote source code
  :if (file-readable-p "/usr/share/asymptote")
  :load-path "/usr/share/asymptote"
  :mode ("\\.asy$" . asy-mode))

;;; ---------------------------------------------------------------------------

(use-package which-key
  ;; display available keybindings in popup
  :ensure
  :init
  (setq which-key-idle-delay 0.5) ; Open after .5s instead of 1s
  :config
  (which-key-setup-side-window-right-bottom)
  (which-key-mode))

(use-package fringe-current-line ; indicate current line on the fringe.
  :ensure
  :config (global-fringe-current-line-mode 1))

;;;----------------------------------------------------------------------------

(use-package neotree ; A tree plugin like NerdTree for Vim
  :ensure
  :bind ([(shift f3)] . neotree-toggle)
  :custom
  (neo-cwd-line-style 'button) ; 'text, 'button
  (neo-show-hidden-files t)
  (neo-smart-open t)
  (neo-theme (if (display-graphic-p) 'icons 'arrow))
                                        ; 'classic, 'nerd, 'ascii, 'arrow, 'icons
                                        ; 'icons requires all-the-icons
  (neo-window-fixed-size nil)
  ;; (neo-window-width 50)
  )

(use-package all-the-icons
  :ensure
  :defer)

(use-package all-the-icons-nerd-fonts
  :ensure
  :after all-the-icons
  :config
  (all-the-icons-nerd-fonts-prefer))

(use-package popwin ; Popup Window Manager
  :ensure
  :config
  (popwin-mode 1)
  ;; (setq display-buffer-function 'popwin:display-buffer)
  ;; (global-set-key (kbd "C-z") popwin:keymap)
  )

(use-package shackle ; Set rules for popup buffers
  :ensure
  ;;:custom (shackle-rules '((grep-mode :select t)))
  :config (shackle-mode 1))

(use-package bnf-mode
  :ensure
  :defer)

;;;----------------------------------------------------------------------------

(use-package slime
  ;; The Superior Lisp Interaction Mode for Emacs
  :ensure
  :defer
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (add-to-list 'slime-contribs 'slime-fancy))

;;;----------------------------------------------------------------------------

(use-package ps-print
  ;; print text from the buffer as PostScript
  :defer

  :bind (([print]           . ps-spool-buffer-with-faces)
         ([(meta print)]    . ps-print-buffer-with-faces)
         ([(meta super p)]  . ps-print-buffer-with-faces)
         ([(shift print)]   . ps-spool-region-with-faces)
         ([(control print)] . ps-despool))
  
  :config

  (setq ps-printer-name           nil
        ps-paper-type             'a4
        ps-top-margin             30
        ps-bottom-margin          30
        ps-left-margin            30
        ps-right-margin           20
        ps-inter-column           30
        ps-print-color-p          t
        ps-font-size              '(9 . 9)
        ps-header-font-size       '(8 . 8)
        ps-header-title-font-size '(7 . 7)
        ps-header-line-pad        0.15
        ps-header-lines           2
        ps-header-offset          15
        ps-print-header-frame     nil
        ps-right-header           '("/pagenumberstring load")
        ps-print-header           t
        ps-show-n-of-n            t
        ps-lpr-command            (expand-file-name "bin/print-preview.sh" user-emacs-directory)
        ps-build-face-reference   t
        )

  (defun ps-spool-to-pdf (beg end &rest ignore)
    (interactive "r")
    (let ((temp-file (concat (make-temp-name "ps2pdf") ".pdf")))
      (call-process-region beg end (executable-find "ps2pdf")
                           nil nil nil "-" temp-file)
      (call-process (executable-find "open") nil nil nil temp-file)))

  ;; (setq ps-print-region-function 'ps-spool-to-pdf)
  )


;; (use-package printing
;;   :defer
;;   :config
;;   (pr-update-menus t))



;;;----------------------------------------------------------------------------

(use-package json-mode
  :ensure
  :defer)

(use-package jsonnet-mode
  :ensure
  :defer)

(use-package php-mode
  :ensure
  :defer)

(use-package udev-mode
  ;; Editing udev rules files 
  :ensure
  :defer)

(use-package remind-bindings
  :ensure
  :disabled
  :bind (("C-c C-d" . 'remind-bindings-toggle-buffer)  ;; toggle buffer
         ("C-c M-d" . 'remind-bindings-specific-mode)) ;; buffer-specific only
  :hook (after-init . remind-bindings-initialise))

(use-package frog-jump-buffer
  ;; EXPERIMENTAL
  ;;
  ;; The fastest buffer-jumping Emacs lisp package around, the
  ;; spiritual successor to ace-jump-buffer, powered by avy via
  ;; frog-menu, allows to hop to any Emacs buffer in 2-3 key strokes
  :ensure
  :bind ("C-c C-b" . frog-jump-buffer))

(use-package define-word
  ;; Display the definition of word at point
  :ensure
  :bind ("C-c d" . define-word-at-point)
        ("C-c D" . define-word))

(use-package clean-buffers :ensure)

(use-package rpm-spec-mode :ensure :defer)

(use-package pkgbuild-mode :ensure :defer)

(use-package dpkg-dev-el
  :ensure
  :defer
  :mode ("/debian/control\\'" . debian-control-mode))

(use-package pacdiff
  :ensure
  :defer)

(use-package mode-icons
  ;; Show icons instead of mode names
  :ensure
  :hook (after-init . mode-icons-mode))

(use-package smart-backspace ; intellj like backspace
  :ensure
  :bind (([?\C-?] . smart-backspace)) )

(use-package sudo-edit
  ;; Utilities for opening files with sudo
  :ensure
  :defer
  ;; :bind (("C-c f s" . sudo-edit))
  )

(use-package etc-sudoers-mode
  ;; Syntax highlighting for the Sudo security policy file, /etc/sudoers
  :ensure
  :defer
  )

(use-package nerd-icons-dired
  ;; Use nerd-icons for Dired. Needs nerd-icons.
  :ensure
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package diredfl
  ;; extra font lock rules for a more colourful dired
  :ensure
  :hook (dired-mode . diredfl-mode))

(use-package dired-subtree
  ;; Insert subdirectories in a tree-like fashion
  :ensure
  :after dired
  ;; :custom
  ;; (dired-subtree-cycle-depth 3) ; default: 3
  ;; (dired-subtree-use-backgrounds t) ; default: t
  :custom-face
  (dired-subtree-depth-1-face ((t (:background "gainsboro"))))
  (dired-subtree-depth-2-face ((t (:background "wheat"))))
  (dired-subtree-depth-3-face ((t (:background "light steel blue"))))
  (dired-subtree-depth-4-face ((t (:background "gray95"))))
  (dired-subtree-depth-5-face ((t (:background "gray90"))))
  (dired-subtree-depth-6-face ((t (:background "gray85"))))
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<C-tab>" . dired-subtree-cycle)
              ("<S-iso-lefttab>" . dired-subtree-remove)))

(use-package dired-du
  ;; display the recursive size of directories in dired
  :ensure
  ;; :hook (dired-mode . dired-du-mode)
  :after dired)

(use-package dired-hist
  ;; Traverse Dired buffer's history: back, forward
  :ensure
  :after dired)


;; dired
;; C-x C-q		dired-toggle-read-only
;; C-c C-c		wdired-finish-edit
;; 0 w                  dired-copy-filename-as-kill


(use-package string-inflection
  ;; underscore -> UPCASE -> CamelCase conversion of names
  :ensure
  :bind (("C-c i" . string-inflection-all-cycle)))


;;HERE
;; ;; Support ligatures from PragmataPro font in Emacs
;; ;; https://github.com/fabrizioschiavi/pragmatapro
;; (use-package "pragmatapro-prettify-symbols-v0.827"
;;   :hook
;;   (prog-mode . add-pragmatapro-prettify-symbols-alist)
;;   :init
;;   (global-prettify-symbols-mode +1))

(use-package counsel-ffdata
  ;; access Firefox bookmarks and history with ivy interface
  ;; needs sqlite3 binary
  :ensure
  :bind (("C-c F h" . counsel-ffdata-firefox-history)
         ("C-c F b" . counsel-ffdata-firefox-bookmarks)))

;; ;; Ligature in fonts
;; ;; Using composition char table
;; ;; https://github.com/tonsky/FiraCode/issues/42#issuecomment-450403454
;; ;; https://github.com/tonsky/FiraCode/wiki/Emacs-instructions

;; (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
;;                (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
;;                (36 . ".\\(?:>\\)")
;;                (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
;;                (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
;;                (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
;;                (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
;;                (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
;;                (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
;;                (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
;;                (48 . ".\\(?:x[a-zA-Z]\\)")
;;                (58 . ".\\(?:::\\|[:=]\\)")
;;                (59 . ".\\(?:;;\\|;\\)")
;;                (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
;;                (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
;;                (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
;;                (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
;;                (91 . ".\\(?:]\\)")
;;                (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
;;                (94 . ".\\(?:=\\)")
;;                (119 . ".\\(?:ww\\)")
;;                (123 . ".\\(?:-\\)")
;;                (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
;;                (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
;;                )
;;              ))
;;   (dolist (char-regexp alist)
;;     (set-char-table-range composition-function-table (car char-regexp)
;;                           `([,(cdr char-regexp) 0 font-shape-gstring]))))

(use-package fontaine
  :ensure
  :demand
  :init
  (setq fontaine-presets
        '((small
           :default-weight regular
           :bold-weight semibold
           :default-height 100)
          (regular
           :default-weight regular
           :bold-weight bold
           :default-height 120)
          (medium
           :default-weight medium
           :bold-weight bold
           :default-height 140)
          (large
           :default-weight medium
           :bold-weight bold
           :default-height 160)
          (presentation
           :default-weight semibold
           :bold-weight extrabold
           :default-height 200)
          (jumbo
           :default-weight semibold
           :bold-weight extrabold
           :default-height 250)
          (t
           :default-family "Iosevka Comfy"
           ;; :default-family "Iosevka Fixed"
           ;; :default-family "Fantasque Sans Mono"
           ;; :default-family "Sudo"
           ;; :default-family "Hack"
           ;; :default-weight regular
           :variable-pitch-family "Iosevka Comfy Motion Duo"
           ;; :variable-pitch-family "Sans Serif"
           ;; :variable-pitch-family "Sudo UI"
           )
          ))
  :config
  ;; Set last preset or fall back to desired style from presets
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  ;; Persist latest font preset when closing/starting Emacs and while switching between themes
  (fontaine-mode 1)
  :bind
  (("C-c f" . fontaine-set-preset)))

(use-package file-info
  ;; Quick view and copy all necessary information about current opened file
  :ensure
  :bind (("C-c C-f" . file-info-show))
  :config
  (setq hydra-hint-display-type 'posframe)
  (setq hydra-posframe-show-params `(:poshandler posframe-poshandler-frame-center
                                     :internal-border-width 2
                                     :internal-border-color "#61AFEF"
                                     :left-fringe 16
                                     :right-fringe 16)))

(use-package titlecase
  :ensure
  :defer
  :config
  (dolist (word '("\\bda\\b" "\\bdas\\b" "\\bde\\b" "\\bdo\\b" "\\bdos\\b"))
    (add-to-list 'titlecase-skip-words-regexps word)))

;;;----------------------------------------------------------------------------

(define-minor-mode my/global-mode
  "A mode that activates custom's bindings."
  t nil my/mode-map
  :global t)

;;;----------------------------------------------------------------------------

(use-package vala-mode
  :disabled ;;;;;; TODO is giving error about vala-multiline-strings
  :ensure
  :defer)

;;----------------------------------------------------------------------------
;; Starts server for (among others) emacsclient
;;----------------------------------------------------------------------------

(use-package server
  :preface (autoload 'server-running-p "server" nil nil)
  :unless (or noninteractive (daemonp) (server-running-p))
  :defer
  :config
  (message "Starting server at %s..." (current-time-string))
  (server-start))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file (locate-user-emacs-file "custom.el"))

(when (file-exists-p custom-file)
  (load custom-file))

;;----------------------------------------------------------------------------
;; Reports init completion time
;;----------------------------------------------------------------------------

(add-hook 'after-init-hook
          (lambda ()
            (message "Emacs init completed in %s with %d garbage collections."
                     (emacs-init-time)
                     gcs-done)
            ))

;; ;; Use a hook so the message doesn't get clobbered by other messages.
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (message "Emacs ready (emacs-startup-hook) in %s with %d garbage collections."
;;                      ;; (float-time (time-subtract after-init-time before-init-time))
;;                      (emacs-init-time)
;;                      gcs-done)))


(provide 'init)
;;; init.el ends here

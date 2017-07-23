;;; -*- Mode: Emacs-Lisp; coding: utf-8; -*-

;; annotation:
;;
;; T k
;; C-M-k
;;   Mark all articles in the current (sub-)thread as read
;;   (gnus-summary-kill-thread). If the prefix argument is positive,
;;   remove all marks instead. If the prefix argument is negative,
;;   tick articles instead.


(setq gnus-verbose 20)		;; default: 7

(setq user-mail-address "j.romildo@gmail.com")
(setq user-full-name "José Romildo Malaquias")

(setq gnus-select-method '(nntp "localhost"))

;; (setq gnus-secondary-select-methods
;;  '((nntp "news.mozdev.org")))

;; (setq gnus-secondary-select-methods
;;  '((nnimap "localhost"
;; 	   (nnimap-address "localhost")
;; 	   ;(nnimap-port 143)
;; 	   ;(nnimap-stream ssl)
;; 	   ;(nnimap-username "romildo")
;; 	   ;(nnimap-password "????????")
;; 	   (nnimap-expunge-on-close 'ask)
;; 	   (nnimap-list-pattern ("INBOX" "Mail/redhat/*" ))
;; 	   )))


(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-number
	gnus-thread-sort-by-author
	gnus-thread-sort-by-subject
	gnus-thread-sort-by-date))




(defun romildo-unicode-threads ()
  (interactive)
  (copy-face 'font-lock-variable-name-face 'gnus-face-6)
  (setq gnus-face-6 'gnus-face-6)
  (copy-face 'font-lock-constant-face 'gnus-face-7)
  (setq gnus-face-7 'gnus-face-7)
  (copy-face 'gnus-face-7 'gnus-summary-normal-unread)
  (copy-face 'font-lock-constant-face 'gnus-face-8)
  (set-face-foreground 'gnus-face-8 "gray50")
  (setq gnus-face-8 'gnus-face-8)
  (copy-face 'font-lock-constant-face 'gnus-face-9)
  (set-face-foreground 'gnus-face-9 "gray70")
  (setq gnus-face-9 'gnus-face-9)
  ;;
  ;; (setq gnus-summary-line-format "%[%U%R%z%]|%&user-date|%«%(%-23,23f%)%»%* %B%s\n")
  ;; (setq gnus-summary-line-format "%[%U%R%z%]|%10&user-date;|%(%-23,23f%)|%*%B%s\n")
  ;; (setq gnus-summary-line-format "%[%U%R%z%]│%10&user-date;│%(%-23,23f%)│%*%B%s\n")
  (setq gnus-summary-line-format "%8{%U%R%z%}│%10&user-date;│%(%-23,23f%)│%*%B%s\n"
        gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M")))

  ;; (setq gnus-summary-same-subject "")

  ;; (setq gnus-sum-thread-tree-root " >")
  ;; (setq gnus-sum-thread-tree-single-indent "  ")
  ;; (setq gnus-sum-thread-tree-vertical "|")
  ;; (setq gnus-sum-thread-tree-indent " ")
  ;; (setq gnus-sum-thread-tree-leaf-with-other "+-> ")
  ;; (setq gnus-sum-thread-tree-single-leaf "`-> ")

  ;;(setq gnus-sum-thread-tree-root nil)
  ;; (setq gnus-sum-thread-tree-single-indent "  ")
  ;; (setq gnus-sum-thread-tree-vertical "|")
  ;; (setq gnus-sum-thread-tree-indent " ")
  ;; (setq gnus-sum-thread-tree-leaf-with-other "+-> ")
  ;; (setq gnus-sum-thread-tree-single-leaf "`-> ")

  ;; (setq gnus-thread-indent-level 3)

  (setq gnus-sum-thread-tree-root "■ ")
  (setq gnus-sum-thread-tree-false-root "□ ")
  (setq gnus-sum-thread-tree-single-indent "▣ ")
  (setq gnus-sum-thread-tree-leaf-with-other "├► ")
  (setq gnus-sum-thread-tree-vertical "│ ")
  (setq gnus-sum-thread-tree-single-leaf "╰► ")
  (setq gnus-sum-thread-tree-indent "  "))

;; http://eschulte.github.com/emacs-starter-kit/starter-kit-gnus.html
;; http://groups.google.com/group/gnu.emacs.gnus/browse_thread/thread/a673a74356e7141f
(defun dan-unicode-threads ()
  (interactive)
  (when window-system
    (setq gnus-sum-thread-tree-indent "  ")
    (setq gnus-sum-thread-tree-root "") ;; "● ")
    (setq gnus-sum-thread-tree-false-root "") ;; "◯ ")
    (setq gnus-sum-thread-tree-single-indent "") ;; "◎ ")
    (setq gnus-sum-thread-tree-vertical        "│")
    (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
    (setq gnus-sum-thread-tree-single-leaf     "╰─► "))
  (setq gnus-summary-line-format
        (concat
         "%0{%U%R%z%}"
         "%3{│%}" "%1{%d%}" "%3{│%}" ;; date
         "  "
         "%4{%-20,20f%}"               ;; name
         "  "
         "%3{│%}"
         " "
         "%1{%B%}"
         "%s\n"))
  (setq gnus-summary-display-arrow t))

;; http://www.emacswiki.org/emacs/TomRauchenwald
(defun oxy-unicode-threads ()
  (interactive)
  (setq gnus-summary-dummy-line-format "    %8{│%}   %(%8{│%}                       %7{│%}%) %6{□%}  %S\n"
	gnus-summary-line-format "%8{%4k│%}%9{%U%R%z%}%8{│%}%*%(%-23,23f%)%7{│%} %6{%B%} %s\n"
	gnus-sum-thread-tree-indent " "
	gnus-sum-thread-tree-root "■ "
	gnus-sum-thread-tree-false-root "□ "
	gnus-sum-thread-tree-single-indent "▣ "
	gnus-sum-thread-tree-leaf-with-other "├─▶ "
	gnus-sum-thread-tree-vertical "│"
	gnus-sum-thread-tree-single-leaf "└─▶ "))
(defun oxy-unicode-threads-heavy ()
  (interactive)
  (copy-face 'font-lock-variable-name-face 'gnus-face-6)
  (setq gnus-face-6 'gnus-face-6)
  (copy-face 'font-lock-constant-face 'gnus-face-7)
  (setq gnus-face-7 'gnus-face-7)
  (copy-face 'gnus-face-7 'gnus-summary-normal-unread)
  (copy-face 'font-lock-constant-face 'gnus-face-8)
  (set-face-foreground 'gnus-face-8 "gray50")
  (setq gnus-face-8 'gnus-face-8)
  (copy-face 'font-lock-constant-face 'gnus-face-9)
  (set-face-foreground 'gnus-face-9 "gray70")
  (setq gnus-face-9 'gnus-face-9)
  (setq gnus-summary-make-false-root 'dummy)
  (setq gnus-summary-make-false-root-always nil)
  ;;
  (setq gnus-summary-line-format "%8{%4k│%}%9{%U%R%z%}%8{│%}%*%(%-23,23f%)%7{║%} %6{%B%} %s\n"
	gnus-summary-dummy-line-format "    %8{│%}   %(%8{│%}                       %7{║%}%) %6{┏○%}  %S\n"
	gnus-sum-thread-tree-indent " "
	gnus-sum-thread-tree-root "┏● "
	gnus-sum-thread-tree-false-root " ○ "
	gnus-sum-thread-tree-single-indent " ● "
	gnus-sum-thread-tree-leaf-with-other "┣━━❯ "
	gnus-sum-thread-tree-vertical "┃"
	gnus-sum-thread-tree-single-leaf "┗━━❯ "))

(romildo-unicode-threads)
;; (oxy-unicode-threads-heavy)




;; (setq gnus-use-trees t)
;; (setq gnus-generate-tree-function 'gnus-generate-horizontal-tree)
;; (setq gnus-tree-minimize-window nil)
;; (gnus-add-configuration
;;  '(article
;;    (vertical 1.0
;; 	     (horizontal 0.25
;; 			 (summary 0.75 point)
;; 			 (tree 1.0))
;; 	     (article 1.0))))

;; do not hide killed threads automatically
(setq gnus-thread-hide-killed nil)


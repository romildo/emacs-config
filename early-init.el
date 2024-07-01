;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;;; Commentary:

;; Emacs 27 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens.

;;; Code:

(message "Starting emacs at %s..." (current-time-string))

;; Use .el files over .elc if they are newer
(setq load-prefer-newer t)

;; Temporarily reduce garbage collection during startup
(defconst jrm/initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; (setq gc-cons-threshold jrm/initial-gc-cons-threshold)
            (setq gc-cons-threshold (* 16 1024 1024)) ; 16mb
            ))

;; Frame sizes can increase/decrease by one pixel
(setq frame-resize-pixelwise t)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Initial and default frame size
(mapc (lambda (var)
        (add-to-list var '(width . 100))
        (add-to-list var '(height . 32)))
      '(default-frame-alist initial-frame-alist))

;; Disable some GUI elements
(menu-bar-mode 0)
(tool-bar-mode 0)
;; (scroll-bar-mode 0)

;; (setq use-dialog-box t)                 ; only for mouse events
;; (setq use-file-dialog nil)

(setq inhibit-startup-echo-area-message user-login-name) ; read the docstring ?????
(setq inhibit-startup-screen t)  ; don't show splash screen
;; (setq inhibit-startup-buffer-menu t)

;; Make installed packages available when Emacs starts, before reading
;; the init file (but after reading the early init file)
;; (setq package-enable-at-startup t)

(setq native-comp-async-report-warnings-errors 'silent)

(provide 'early-init)

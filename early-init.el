;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;;; Commentary:

;; Emacs 27 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens.

;;; Code:

(message "Starting emacs at %s..." (current-time-string))

;; Temporarily reduce garbage collection during startup
(defconst jrm/initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold jrm/initial-gc-cons-threshold)))

;; Frame sizes can increase/decrease by one pixel
(setq frame-resize-pixelwise t)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Disable some GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
;; (scroll-bar-mode -1)

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

(setq load-path (cons "~/.elisp/clean-mode" load-path))
(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.icl$"  . clean-mode)
                ("\\.dcl$"  . clean-mode))))
(autoload 'clean-mode "clean-mode"
   "Major mode for editing Clean scripts." t)

;;; Scheme

(require 'init-lispy)
(require 'init-rainbow)

(add-hook 'scheme-mode-hook 'ambrevar/turn-on-prettify-before-save)
(add-hook 'scheme-mode-hook 'ambrevar/turn-on-complete-filename)
(add-hook 'scheme-mode-hook 'ambrevar/turn-on-tab-width-to-8) ; Because some existing code uses tabs.
(add-hook 'scheme-mode-hook 'ambrevar/turn-off-indent-tabs)   ; Should not use tabs.
(add-hook 'scheme-mode-hook 'ambrevar/init-lispy)
(when (fboundp 'rainbow-delimiters-mode)
  (add-hook 'scheme-mode-hook #'rainbow-delimiters-mode))

(with-eval-after-load 'scheme
  (when (require 'patch-scheme nil t)
    (setq scheme-imenu-generic-expression
          al/scheme-imenu-generic-expression)
    (advice-add 'scheme-indent-function
                :override 'al/scheme-indent-function)
    (add-hook 'scheme-mode-hook 'al/scheme-fix-docstring-font-lock)))

(with-eval-after-load 'geiser-impl
  (helm-defswitcher
   "Geiser-REPL"
   (lambda (b)
     (with-current-buffer b
       (derived-mode-p 'geiser-repl-mode)))
   (lambda (&optional candidate)
     (defun ambrevar/geiser-repl-buffer-name (impl)
       (format "* %s%s *" (geiser-repl--repl-name impl)
               (if candidate (format "-%s" candidate) "")))
     (advice-add 'geiser-repl-buffer-name :override 'ambrevar/geiser-repl-buffer-name)
     (call-interactively 'run-geiser)
     (advice-remove 'geiser-repl-buffer-name 'ambrevar/geiser-repl-buffer-name)))

  ;; (setq geiser-repl-skip-version-check-p t
  ;;       geiser-mode-start-repl-p t)
  (setq geiser-active-implementations (delq 'chicken geiser-active-implementations)
        geiser-default-implementation 'guile
        ;; geiser-repl-save-debugging-history-p t
        geiser-repl-history-size 5000)
  (add-hook 'geiser-repl-mode-hook 'lispyville-mode)
  (add-hook 'geiser-repl-mode-hook 'rainbow-delimiters-mode))

(with-eval-after-load 'geiser-guile
  (require 'patch-geiser nil 'noerror))

(provide 'init-scheme)

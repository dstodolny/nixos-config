;;; Lisp

(require 'init-lispy)
(require 'init-rainbow)

(add-hook 'lisp-mode-hook 'dnixty/turn-on-prettify-before-save)
(add-hook 'lisp-mode-hook 'dnixty/turn-on-complete-filename)
(add-hook 'lisp-mode-hook 'dnixty/turn-on-tab-width-to-8) ; Because some existing code uses tabs.
(add-hook 'lisp-mode-hook 'dnixty/turn-off-indent-tabs)   ; Should not use tabs.
(add-hook 'lisp-mode-hook 'dnixty/init-lispy)
(when (fboundp 'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode))

;;; Common LISP.
(defun dnixty/slime-switch-to-repl () ; TODO: Replace with `helm-defswitch'.
  (interactive)
  (pcase (length slime-net-processes)
    (0 (slime))
    (1 (if (and (eq (current-buffer) (slime-output-buffer))
                (require 'helm-slime nil 'no-error))
           (helm-slime-mini)
         (pop-to-buffer (slime-output-buffer))))
    (_ (if (require 'helm-slime nil 'noerror)
           (helm-slime-mini)
         (pop-to-buffer (slime-output-buffer))))))

(with-eval-after-load 'slime
  (setq slime-lisp-implementations
        '((sbcl ("sbcl" "--noinform"))
          (ccl ("ccl"))))
  (let ((slime-extra '(slime-fancy
                       ;; slime-banner
                       slime-xref-browser
                       ;; slime-highlight-edits ; A bit slow...
                       slime-sprof
                       slime-quicklisp
                       slime-asdf
                       slime-indentation)))
    ;; slime-company should not be required, see
    ;; https://github.com/anwyn/slime-company/issues/11.
    (when (require 'slime-repl-ansi-color nil t)
      (add-to-list 'slime-extra 'slime-repl-ansi-color)
      (setq slime-repl-ansi-color t))
    (when (ignore-errors (find-library-name "slime-company"))
      (add-to-list 'slime-extra 'slime-company))
    (slime-setup slime-extra)
    (add-hook 'slime-repl-mode-hook 'dnixty/init-lispy)))

;; Read CLHS locally.
(or
 ;; Quicklisp package.
 (load "~/.quicklisp/clhs-use-local.el" 'noerror)
 ;; Unofficial Guix package (non-free license).
 (when (require 'clhs nil 'noerror)
   (clhs-setup)))

(provide 'init-lisp)

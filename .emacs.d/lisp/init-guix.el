(defvar dnixty/guix-checkout-directory (expand-file-name "~/projects/guix"))

(with-eval-after-load 'geiser-guile
  (when (require 'yasnippet nil t)
    (yas-global-mode 1)
    ;; This is not enough since COMMIT_MSG is not in scheme-mode.
    ;; TODO: Add to find-file-hook instead and check if parent folder is ~/projects/guix.
    ;; (add-hook 'scheme-mode-hook 'yas-minor-mode)
    (with-eval-after-load 'yasnippet
      (add-to-list 'yas-snippet-dirs
                   (expand-file-name "etc/snippets"
                                     dnixty/guix-checkout-directory))))
  (add-to-list 'geiser-guile-load-path dnixty/guix-checkout-directory))

;; To use package declaration from the local checkout:
;; (setq guix-load-path dnixty/guix-checkout-directory)

(defun dnixty/init-guix ()
  (and buffer-file-name
       (string-match "\\<guix\\>" buffer-file-name)
       (guix-devel-mode)))
(add-hook 'scheme-mode-hook 'dnixty/init-guix)

(defun dnixty/guix-debbugs-gnu (&optional severities packages archivedp suppress tags)
  "Like `debbugs-gnu' but for the Guix project."
  (interactive)
  (let ((debbugs-gnu-default-packages '("guix-patches" "guix")))
    (if (called-interactively-p)
        (call-interactively 'debbugs-gnu)
      (debbugs-gnu severities packages archivedp suppress tags))))

(provide 'init-guix)

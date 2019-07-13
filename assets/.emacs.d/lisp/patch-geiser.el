;; See https://gitlab.com/jaor/geiser/issues/252.
(defun guile--manual-look-up (id mod)
  (let ((info-lookup-other-window-flag
         geiser-guile-manual-lookup-other-window-p))
    (info-lookup-symbol (symbol-name id) 'geiser-guile-mode))
  (when geiser-guile-manual-lookup-other-window-p
    (switch-to-buffer-other-window "*info*"))
  (search-forward (format "%s" id) nil t))

;; See https://github.com/emacs-helm/helm/issues/2134 and
;; https://gitlab.com/jaor/geiser/issues/180
(when (>= emacs-major-version 26)
  (setq geiser-completion--module-list-func
        (completion-table-dynamic 'geiser-completion--module-list)))

(provide 'patch-geiser)

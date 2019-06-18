;;; Emacs config

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Prerequisites

(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error  "Your Emacs is too old -- this config requires v%s or higher" minver)))

;;; Speed up init.
;;; Temporarily reduce garbage collection during startup. Inspect `gcs-done'.
(defun dnixty/reset-gc-cons-threshold ()
  (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))))
(setq gc-cons-threshold (* 64 1024 1024))
(add-hook 'after-init-hook 'dnixty/reset-gc-cons-threshold)
;;; Temporarily disable the file name handler.
(setq default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun dnixty/reset-file-name-handler-alist ()
  (setq file-name-handler-alist default-file-name-handler-alist))
(add-hook 'after-init-hook 'dnixty/reset-file-name-handler-alist)

;;; Avoid the "loaded old bytecode instead of newer source" pitfall.
(setq load-prefer-newer t)

;;; Store additional config in a 'lisp' subfolder and add it to the load path so
;;; that `require' can find the files.
;;; This must be done before moving `user-emacs-directory'.
(add-to-list 'load-path (expand-file-name  "lisp/" user-emacs-directory))

;;; Move user-emacs-directory  so that user files don't mix with cache files.
(setq user-emacs-directory "~/.cache/emacs/")

;; Tor / Proxy: set up before package initialization.
(when (member "privoxy"
              (mapcar (lambda (p) (alist-get 'comm (process-attributes p)))
                      (list-system-processes)))
  (require 'url)
  (setq url-proxy-services
        '(("http" . "127.0.0.1:8118")
          ("https" . "127.0.0.1:8118"))))

(when (require 'package nil t)
  ;; Different Emacs version have different byte code. If a versioned ELPA
  ;; directory is found, use it.
  (let ((versioned-dir (format "elpa-%s.%s" emacs-major-version emacs-minor-version)))
    (when (member versioned-dir (directory-files (expand-file-name ".." package-user-dir)))
      (setq package-user-dir (expand-file-name (concat "../" versioned-dir) package-user-dir))))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  (package-initialize))

;;; Site Lisp folder for local packages and development.
;; We need to roll it out manually since we want it first in the `load-path',
;; while `normal-top-level-add-subdirs-to-load-path' appends it to the very end.
(defun dnixty/package-refresh-load-path (path)
  "Add every non-hidden sub-folder of PATH to `load-path'."
  (when (file-directory-p path)
    (dolist (dir (directory-files path t "^[^\\.]"))
      (when (file-directory-p dir)
        (setq load-path (add-to-list 'load-path dir))
        (dolist (subdir (directory-files dir t "^[^\\.]"))
          (when (file-directory-p subdir)
            (setq load-path (add-to-list 'load-path subdir))))))))
(let ((site-lisp (expand-file-name "site-lisp/" "~/.local/share/emacs/")))
  (add-to-list 'load-path site-lisp)
  (dnixty/package-refresh-load-path site-lisp))

;;; Local config. See below for an example usage.
(load "local-before" t)

(require 'functions)
(require 'hook-functions)
(require 'main)
(require 'visual)

;;; ChangeLog
(defun dnixty/change-log-set-indent-rules ()
  (setq tab-width 2 left-margin 2))
(add-hook 'change-log-mode-hook 'dnixty/change-log-set-indent-rules)

;;; Completion
(nconc package-selected-packages '(company helm-company slime-company))
(when (require 'company nil t)
  (setq company-idle-delay 0))

;;; CSV
(nconc package-selected-packages '(csv-mode))

;;; Daemons.
(nconc package-selected-packages '(daemons))

;;; Diff
;;; TODO: In diff-mode, both `[[` and `C-M-a` do not go back to previous index
;;; once they are at the beginning of an index.
(nconc package-selected-packages '(ztree))

;;; Dired
(nconc package-selected-packages '(disk-usage))
;;; Dired is loaded after init.el, so configure it only then.
(with-eval-after-load 'dired (require 'init-dired))

;;; Eshell
;;; Extend completion.
(nconc package-selected-packages '(fish-completion bash-completion
                                   pcomplete-extension pcmpl-args pcmpl-git))
(nconc package-selected-packages '(esh-autosuggest))
(with-eval-after-load 'eshell (require 'init-eshell))
(autoload 'helm-eshell-switch "eshell")

;;; Eww
(nconc package-selected-packages '(helm-eww))
(with-eval-after-load 'eww (require 'init-eww))
(autoload 'helm-eww-switch "eww")

;;; Expand region.
(nconc package-selected-packages '(expand-region))
(when (require 'expand-region nil t)
  (global-set-key (kbd "C-=") 'er/expand-region))

;;; Guix
(when (executable-find "guix")
  (nconc package-selected-packages '(guix))
  (require 'init-guix))

;;; Haskell
(nconc package-selected-packages '(haskell-mode))
(when (require 'haskell-mode nil t) (require 'init-haskell))

;;; Helm
(nconc package-selected-packages '(helm helm-descbinds helm-ls-git))
(when (require 'helm-config nil t) (require 'init-helm))

(nconc package-selected-packages '(helpful))
(when (require 'helpful nil t)
	(global-set-key (kbd "C-h f") #'helpful-callable)
	(global-set-key (kbd "C-h v") #'helpful-variable)
	(global-set-key (kbd "C-h o") #'helpful-at-point)
	(global-set-key (kbd "C-h F") #'helpful-function)
	(global-set-key (kbd "C-h c") #'helpful-key))

;;; Hex editing
(nconc package-selected-packages '(nhexl-mode))

;;; Highlight
(nconc package-selected-packages '(hl-todo))
(when (require 'hl-todo nil t)
	(add-to-list 'hl-todo-keyword-faces `("REVIEW" . ,(alist-get "TODO" hl-todo-keyword-faces nil nil 'equal)))
	(global-hl-todo-mode)
	;; (global-set-key (kbd "M-s M-o") 'hl-todo-occur)
	(define-key hl-todo-mode-map (kbd "M-s t") 'hl-todo-occur))

;;; Iedit
(nconc package-selected-packages '(iedit))
(when (require 'iedit nil t)
	(global-set-key (kbd "C-;") 'iedit-mode))

;;; Image
(nconc package-selected-packages '(image+))
(with-eval-after-load 'image
  (setq image-animate-loop t)
  (add-hook 'image-mode-hook 'image-toggle-animation)
  (require 'image+ nil t))

;;; IRC
(nconc package-selected-packages '(circe))
(with-eval-after-load 'circe (require 'init-circe))

;;; Lisp
(nconc package-selected-packages '(lispy lispyville rainbow-delimiters geiser slime))
(with-eval-after-load 'lisp-mode (require 'init-lisp))
(with-eval-after-load 'scheme (require 'init-scheme))
(setq geiser-repl-history-filename (expand-file-name "geiser_history" user-emacs-directory))
;;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook 'dnixty/turn-on-prettify-before-save)
(add-hook 'emacs-lisp-mode-hook 'dnixty/turn-on-complete-filename)
(add-hook 'emacs-lisp-mode-hook 'dnixty/turn-on-tab-width-to-8) ; Because some existing code uses tabs.
(add-hook 'emacs-lisp-mode-hook 'dnixty/turn-off-indent-tabs)   ; Should not use tabs.
(add-hook 'emacs-lisp-mode-hook 'dnixty/init-lispy)
(when (fboundp 'rainbow-delimiters-mode)
	(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))
(dnixty/define-keys emacs-lisp-mode-map
        "<f5>" 'package-lint-current-buffer
	;; Do not use `recompile' since we want to change the compilation folder for the current buffer.
	"<f6>" (lambda () (interactive) (async-byte-recompile-directory (file-name-directory (buffer-file-name)))))

;;; Lua
(nconc package-selected-packages '(lua-mode))
(with-eval-after-load 'lua-mode (require 'init-lua))

;;; Magit
;;; Magit can be loaded just-in-time.
(nconc package-selected-packages '(magit magit-todos orgit))
(with-eval-after-load 'magit
  (setq auto-revert-mode-text "")
  (set-face-foreground 'magit-branch-remote "#d26937 #c23127")
  (setq git-commit-summary-max-length fill-column)
  ;; Customize what to fold by default.
  ;; (push (cons [* commitbuf] 'hide) magit-section-initial-visibility-alist)
  ;; Avoid conflict with WM.
  (define-key magit-mode-map (kbd "s-<tab>") nil)
  (setq magit-diff-refine-hunk 'all)
  (setq magit-repository-directories '(("~/projects" . 1)))

  (when (require 'magit-todos nil t)
    ;; REVIEW: Default scanner does not work on Guix because Git needs be compiled with PCRE.
    (setq magit-todos-scanner #'magit-todos--scan-with-find|grep)
    (magit-todos-mode)))
(when (fboundp 'magit-status)
  (global-set-key (kbd "C-x g") 'magit-status))
(with-eval-after-load 'orgit
  (setq orgit-store-repository-id t))

;;; Markdown
(nconc package-selected-packages '(markdown-mode))
(with-eval-after-load 'markdown-mode (require 'init-markdown))

;;; Matlab / Octave
;; (add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode)) ; matlab, but conflicts with obj-c.
(defun dnixty/octave-set-comment-start ()
  "Set comment character to '%' to be Matlab-compatible."
  (set (make-local-variable 'comment-start) "% "))
(add-hook 'octave-mode-hook 'dnixty/octave-set-comment-start)

;;; News
(nconc package-selected-packages '(elfeed hackernews))
(with-eval-after-load 'elfeed (require 'init-elfeed))
(autoload 'helm-elfeed-switch "elfeed")

;;; Org-mode
(nconc package-selected-packages '(org-plus-contrib org-bullets helm-org-contacts)) ; org-plus contains latest Org mode.
(with-eval-after-load 'org (require 'init-org))
(autoload 'helm-org-switch "org")

;;; Pass
(nconc package-selected-packages '(helm-pass))

;;; PDF
;;; pdf-tools requires poppler built with cairo support.
;;; We cannot defer loading as `pdf-tools-install' is required for PDF
;;; association.
;;; REVIEW: `save-place' does not seem to work with pdf-tools.
;;; See https://github.com/politza/pdf-tools/issues/18.
(nconc package-selected-packages '(pdf-tools))
(when (require 'pdf-tools nil t)
  (pdf-tools-install t t t))

;;; Pinentry
(nconc package-selected-packages '(pinentry)) ; pinentry.el was removed from Emacs 26

;;; Python
(with-eval-after-load 'python (require 'init-python))

;;; Rainbow-mode
(nconc package-selected-packages '(rainbow-mode))
(when (require 'rainbow-mode nil t)
  (dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
    (add-hook hook 'rainbow-mode)))

;;; Shell
(with-eval-after-load 'sh-script (require 'init-sh))
;;; Arch Linux PKGBUILD
(add-to-list 'auto-mode-alist '("PKGBUILD" . sh-mode))

;;; Syntax checking
(nconc package-selected-packages '(flycheck helm-flycheck))
(when (require 'flycheck nil t) (require 'init-flycheck))

;;; System packages
(nconc package-selected-packages '(helm-system-packages))
(global-set-key (kbd "C-x c #") 'helm-system-packages)

;;; Terminal
(with-eval-after-load 'term
  (setq term-buffer-maximum-size 0))

;;; Typescript
(nconc package-selected-packages '(tide web-mode css-mode js2-mode typescript-mode))
(when (require 'tide nil t) (require 'init-typescript))

;;; Window manager
(nconc package-selected-packages '(exwm helm-exwm))
(nconc package-selected-packages '(pulseaudio-control))
(with-eval-after-load 'pulseaudio-control
	(setq pulseaudio-control-use-default-sink t
				pulseaudio-control-volume-step "2%"))
(with-eval-after-load 'exwm
  (require 'init-exwm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Finalization

;; Don't let `customize' clutter my config.
(setq custom-file
      (if (boundp 'server-socket-dir)
	  (expand-file-name "custom.el" server-socket-dir)
	(expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)


;;; Local config. You can use it to set system specific variables, such as the
;;; external web browser or the geographical coordinates:
;;
;; (setq calendar-latitude 20.2158)
;; (setq calendar-longitude 105.938)
(load "local-after" t)

;;; Main options

(require 'functions)

;;; Minimal UI. Run early to hide it as soon as possible.
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
;;; `tool-bar-mode' and `scroll-bar-mode' might not be compiled in.
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;;; In some cases, Emacs can still decide by itself to use graphical boxes.
;;; Force on using the minibuffer instead.
(setq use-dialog-box nil)

;;; Timeout before echoing the prefix of an unfinished keystroke.
(setq echo-keystrokes 0.5)

;;; Remember last cursor position.
(save-place-mode)
;;; When the daemon is killed abruptly, places are not saved. Adding this hook
;;; allows to save places at a strategic moment.
(add-hook 'before-save-hook 'save-place-kill-emacs-hook)

;;; Recent files
(setq recentf-max-saved-items 100)

;; Save M-: history.
(savehist-mode)

;;; Disable autosave features.
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

;;; Place backup files in specifi directory.
(setq backup-directory-alist
			`(("." . ,(expand-file-name "backups" user-emacs-directory))))

;;; Lockfiles
(setq create-lockfiles nil)

;;; Default mode
(setq-default major-mode 'text-mode)

;;; Disable suspend key since it is useless on Emacs server.
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;;; Make questions less annoying.
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Enable all disabled commands.
(setq disabled-command-function nil)

;;; Print buffer size in mode line.
(size-indication-mode 1)

;;; Line numbers
;;; Adding to `find-file-hook' ensures it will work for every file, regardless of
;;; the mod, but it won't work for buffers without files nor on mode change.
(dolist (hook '(prog-mode-hook text-mode-hook))
	(add-hook hook 'dnixty/turn-on-column-number-mode)
	(add-hook hook 'dnixty/turn-off-line-number-mode)
	(add-hook hook 'display-line-numbers-mode))
(setq display-line-numbers-type 'visual)
(defun dnixty/turn-on-absolute-line-number ()
  (setq display-line-numbers-type t))
(setq auto-revert-interval 1)
(add-hook 'auto-revert-tail-mode-hook 'dnixty/turn-on-absolute-line-number)

;;; Alternative scrolling
(setq scroll-error-top-bottom t)

;;; Kill whole line including \n.
(setq kill-whole-line t)

;;; Indentation
(setq-default tab-width 2)
(defvaralias 'standard-indent 'tab-width)
(setq-default indent-tabs-mode t)

;;; Line by line scrolling
(setq scroll-step 1)

(setq
 whitespace-style
 '(face empty indentation space-after-tab space-before-tab tab-mark trailing))
;;; REVIEW `whitespace-report' will mistakenly always report empty lines at
;;; beginning and end of buffer as long as there is at least one empty line.
;;; `whitespace-cleanup' works properly however.
;;; Reported at http://debbugs.gnu.org/cgi/bugreport.cgi?bug=23740.
;; (setq whitespace-action '(report-on-bogus))

;;; Add formatting functions to the buffer-local `before-save-hook'.
;;; WARNING: this an break some configuration files needing whitespaces at the
;;; end. This can also slow down saving on big files. Some modes (e.g. lisp) run
;;; `dnixty/prettify' in their local hook, which is redundant with this.
;; (add-hook 'find-file-hook 'dnixty/turn-on-prettify-before-save)
(add-hook 'find-file-hook 'dnixty/turn-on-delete-trailing-whitespace)

;;; Cycle spacing instead of just-one-space. This frees M-\.
(global-set-key [remap just-one-space] 'cycle-spacing)

;;; Abbreviation is like snippets: annoying at times, especially in
;;; prog-mode. They are useful in text mode to avoid the sprawling of
;;; abbreviations.
(add-hook 'text-mode-hook 'abbrev-mode)

;;; Auto-fill
(when (getenv "MANWIDTH")
	(setq-default fill-column (string-to-number (getenv "MANWIDTH"))))
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; Enforce horizontal splitting. 140 means that the window is large enough to
;;; old 2 other windows of 70 columns.
(setq split-height-threshold nil
      split-width-threshold 140)

;;; Windmove mode
;;; By default, it allows easy window switching with Shift+arrows. I like to
;;; stick to the home-row, but to avoid shadowing other binding I exceptionaly use
;;; 'super' (normally reserved to the WM).
(when (fboundp 'windmove-default-keybindings)
  (dnixty/global-set-keys
   "s-h" 'windmove-left
   "s-j" 'windmove-down
   "s-k" 'windmove-up
   "s-l" 'windmove-right))
(dnixty/global-set-keys
 "s-o" 'delete-other-windows
 ;; "s-w" 'other-window
 "s-c" 'delete-window)


;; REVIEW: If xdg-open is not found, set Emacs URL browser to the environment browser,
;; or w3m if BROWSER is not set.
;; See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=18986.
;; In Emacs 26, the BROWSER variable is still not checked.
(require 'browse-url)
(setq browse-url-generic-program (or
                                  (executable-find (or (getenv "BROWSER") ""))
                                  (when (executable-find "xdg-mime")
                                    (let ((desktop-browser (dnixty/call-process-to-string "xdg-mime" "query" "default" "text/html")))
                                      (substring desktop-browser 0 (string-match "\\.desktop" desktop-browser))))
                                  (executable-find browse-url-mozilla-program)
                                  (executable-find browse-url-firefox-program)
                                  (executable-find browse-url-chromium-program)
                                  (executable-find browse-url-kde-program)
                                  (executable-find browse-url-conkeror-program)
                                  (executable-find browse-url-chrome-program)))
(setq shr-external-browser browse-url-browser-function)
(setq browse-url-browser-function '(;; TODO: Display hyperspec in other window.
                                    ("http://www.lispworks.com/reference/HyperSpec/.*" . eww-browse-url)
                                    ("file:///.*HyperSpec.*" . eww-browse-url)
                                    ("." . browse-url-default-browser)))

;; shr
(setq shr-width (string-to-number (or (getenv "MANWIDTH") "80"))
      ;; If you're using a dark theme, and the messages are hard to read, it
      ;; can help to change the luminosity, e.g.:
      shr-color-visible-luminance-min 80)


;;; Extend MIME-types support for videos.
(with-eval-after-load 'mailcap
  (dolist (ext '((".webm" . "video/webm")
                 (".mp4" . "video/mp4")
                 (".flv" . "video/mp4")
                 (".ogv" . "video/ogg")
                 (".wmv" . "video/x-ms-wmv")
                 (".mkv" . "video/x-matroska")))
    (add-to-list 'mailcap-mime-extensions ext)))

;;; Default ispell dictionary. If not set, Emacs uses the current locale.
(setq ispell-dictionary "english")
(dnixty/define-keys text-mode-map
                      "C-<f6>" 'ispell-change-dictionary
                      "<f6>" 'ispell-buffer)


;;; Show matching parenthesis
(show-paren-mode 1)
;;; By default, there's a small delay before showing a matching parenthesis. Set
;;; it to 0 to deactivate.
(setq show-paren-delay 0)
(setq show-paren-when-point-inside-paren t)

;;; Spawn terminal shortcut: WM's binding is s+<return>.
(global-set-key (kbd "C-x M-<return>") 'spawn-terminal)

;;; Calendar ISO display
(setq calendar-week-start-day 1)
(setq calendar-date-style 'iso)

;;; Desktop-mode
;;; REVIEW: `desktop-kill' should not query the user in `kill-emacs-hook'.
;;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=28943
;;; TODO: Desktop mode does not save window registers properly.
;;; See https://groups.google.com/forum/#!topic/gnu.emacs.help/64aO_O43530
;;; and https://www.reddit.com/r/emacs/comments/4q38s1/save_register_between_sessions/?st=j419vc7r&sh=2617ffb4
;;; and http://debbugs.gnu.org/cgi/bugreport.cgi?bug=27422
;;; and https://stackoverflow.com/questions/5830494/windows-configuration-to-registers#5830928
;;; and https://www.reddit.com/r/emacs/comments/7au3hj/how_do_you_manage_your_emacs_windows_and_stay_sane/dpfbg3a/?context=3.
(defun dnixty/desktop-setup (&rest _ignored)
  (when (and (fboundp 'server-running-p)
             (server-running-p)
             (or (not (boundp 'desktop-save-mode))
                 (null desktop-save-mode)))
    (when (< emacs-major-version 27)
      ;; By default, Emacs<27 prompts for unsafe variable when loading desktop
      ;; which stucks the daemon.  Disable this behaviour.
      (defun dnixty/enable-safe-local-variables ()
        (setq enable-local-variables t))
      (add-hook 'after-init-hook 'dnixty/enable-safe-local-variables))
    (require 'desktop)                  ; This adds a hook to `after-init-hook'.
    (when (< emacs-major-version 27)
      (defun dnixty/enable-all-local-variables ()
        (setq enable-local-variables :all))
      (add-hook 'after-init-hook 'dnixty/enable-all-local-variables))
    (when (< emacs-major-version 27)
      (load "patch-desktop"))
    (setq history-length 250
          ;; Default timer (30) is way too high: for somebody too frenzy, the timer
          ;; might never be saved.  See
          ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=28943.
          desktop-auto-save-timeout 5
          ;; desktop-restore-eager 4 ; Can be annoying as you don't have your last-loaded buffers immediately.
          ;; desktop-load-locked-desktop 'ask
          desktop-restore-frames nil
          desktop-save t)
    ;; (if (< emacs-major-version 27)
    ;;     (desktop-save-mode)
    ;;   (defun dnixty/desktop-init (_frame)
    ;;     (desktop-save-mode)
    ;;     (desktop-read)
    ;;     (remove-hook 'server-after-make-frame-hook 'dnixty/desktop-init))
    ;;   (add-hook 'server-after-make-frame-hook 'dnixty/desktop-init))
    (desktop-save-mode)
    (desktop-read)
    ;; Don't save crytped files since they can't be restored.
    (setq desktop-files-not-to-save
          (concat (substring desktop-files-not-to-save 0 -2) "\\|" (regexp-quote ".gpg") "\\'\\)"))
    ;; Or don't save any file, for faster startup and less problems.
    (setq desktop-files-not-to-save ".")
    ;; Discarding PDFs and images makes it lighter.
    (add-to-list 'desktop-modes-not-to-save 'pdf-view-mode)
    (add-to-list 'desktop-modes-not-to-save 'image-mode)
    ;; Discard Elfeed since it may ask for PGP passphrase.
    (add-to-list 'desktop-modes-not-to-save 'elfeed-search-mode)
    ;; TODO: `compile-history' should be buffer local but that does not work.
    ;; http://user42.tuxfamily.org/compile-history-local/index.html
    ;; http://stackoverflow.com/questions/22995203/one-compile-command-per-buffer-not-directory
    ;; (add-to-list 'desktop-locals-to-save 'compile-history)
    (add-to-list 'desktop-locals-to-save 'compile-command)
    (add-to-list 'desktop-locals-to-save 'ispell-local-dictionary)))

;; Auto-load/save sessions only when running the daemon.  `server-running-p' is
;; only useful once the daemon is started and cannot be used for initialization.
;; Use `daemonp' instead if emacs is started with `--daemon', or add to the
;; server hook otherwise.
(when (daemonp)
  (dnixty/desktop-setup))
;; If started with `server-start' instead of `--daemon'.
;; server-mode does not seem to have any post-start hook.
(advice-add 'server-start :after #'dnixty/desktop-setup)

;;; Buffer names.
(setq uniquify-buffer-name-style 'forward)

;;; Skeleton settings
(when (require 'patch-skeletons nil 'noerror)
;;; Do not expand abbrevs in skeletons.
  (setq-default skeleton-further-elements '((abbrev-mode nil)))
  (dnixty/turn-on-skeleton-markers)
  (dnixty/global-set-keys
   "C->" 'skeleton-next-position
   "C-<" 'skeleton-previous-position))

;;; Disable prompt (but leave warning) on git symlink.
(setq vc-follow-symlinks t)

;;; Clipboard and primary selection.
(setq select-enable-primary t
      save-interprogram-paste-before-kill t)

;;; Move mouse away.
(mouse-avoidance-mode 'banish)
;;; That binding is not very useful and gets in the way of C-<mouse-1>.
(global-unset-key (kbd "C-<down-mouse-1>"))

;;; Sort
(setq sort-fold-case t)

;;; Replace not-so-useful comment-dwim binding.
(global-set-key (kbd "M-;") 'comment-line)

;;; Eldoc: Disable if too distracting.
;; (global-eldoc-mode 0)
;; (setq eldoc-idle-delay 0.1) ; Could be even more distracting.

;;; Replace `kill-buffer' binding by `kill-this-buffer'.
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;;; Ediff
;;; TODO: Ediff does not seem to auto-refine.  Bug?  Compare daemon and no-daemon.
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

;;; Trash
(setq delete-by-moving-to-trash t)

;;; Display Time World
(setq
 zoneinfo-style-world-list
 '(("UTC" "-")
   ("Europe/Paris" "France Germany Sweden")
   ("Asia/Calcutta" "India")
   ("Indian/Mauritius" "Mauritius")
   ("Africa/Tunis" "Tunisia")
   ("Asia/Ho_Chi_Minh" "Vietnam")
   ("Australia/Melbourne" "Melbourne")
   ("Africa/Nairobi" "Uganda")))

;;; Frame title
(setq frame-title-format (concat "%b" (unless (daemonp) " [serverless]")))

;;; Initial scratch buffer message.
;; (require 'functions) ; For `dnixty/fortune-scratch-message'.
;; (let ((fortune (dnixty/fortune-scratch-message)))
;;   (when fortune
;;     (setq initial-scratch-message fortune)))

;;; Support for Emacs pinentry.
;;; Required for eshell/sudo and everything relying on GPG queries.
(setq epa-pinentry-mode 'loopback) ; This will fail if gpg>=2.1 is not available.
(when (require 'pinentry nil t)
  (pinentry-start))

;;; Edebug
;; (setq
;;  ;; REVIEW: Does not seem necessary, since '=' already displays the coverage.
;;  edebug-test-coverage t
;;  edebug-trace t)

;;; Make windowing more reactive on.  This is especially true with Helm on EXWM.
(when (= emacs-major-version 26)
  (setq x-wait-for-event-timeout nil))

(setq woman-fill-column fill-column)

;;; Save all visited URLs.
(setq url-history-track t
      url-history-file (expand-file-name "url/history" user-emacs-directory))

(setq abbrev-file-name (expand-file-name "abbrev_defs" "~/personal"))

;; Initialise in *scratch* with org-mode
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

(provide 'main)

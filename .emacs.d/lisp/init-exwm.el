;;; EXWM

;;; When stating the client from .xinitrc, `save-buffer-kill-terminal' will
;;; force-kill Emacs before it can run through `kill-emacs-hook'.
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)

;;; Athena+Xaw3d confuses xcape when binding Caps-lock to both L_Ctrl
;;; escape, in which case it will procude <C-escape> in Emacs. In practice, it
;;; means that `C-` keys will works but `<escape>` will need a fast double tap
;;; on Caps Lock.
;;;
;;; See https://github.com/ch11ng/exwm/issues/285
;;; and https://gitlab.com/interception/linux/plugins/caps2esc/issues/2.

;;; Rename buffer to window title.
(defun dnixty/exwm-rename-buffer-to-title () (exwm-workspace-rename-buffer exwm-title))
(add-hook 'exwm-update-title-hook 'dnixty/exwm-rename-buffer-to-title)

(add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
(add-hook 'exwm-floating-exit-hook 'exwm-layout-show-mode-line)

;;; Allow non-floating resizing with mouse.
(setq window-divider-default-bottom-width 2
      window-divider-default-right-width 2)
(window-divider-mode)

;;; System tray
(require 'exwm-systemtray)
(exwm-systemtray-enable)
(setq exwm-systemtray-height 16)

;;; Those cannot be set globally: if Emacs would be run in another WM, the "s-"
;;; prefix will conflict with the WM bindings.
(exwm-input-set-key (kbd "s-R") #'exwm-reset)
(exwm-input-set-key (kbd "s-x") #'exwm-input-toggle-keyboard)
(exwm-input-set-key (kbd "s-h") #'windmove-left)
(exwm-input-set-key (kbd "s-j") #'windmove-down)
(exwm-input-set-key (kbd "s-k") #'windmove-up)
(exwm-input-set-key (kbd "s-l") #'windmove-right)
(exwm-input-set-key (kbd "s-D") #'kill-this-buffer)
(exwm-input-set-key (kbd "s-b") #'list-buffers)
(exwm-input-set-key (kbd "s-f") #'find-file)

(when (require 'functions)
  (exwm-input-set-key (kbd "s-\\") 'dnixty/toggle-window-split)
  (exwm-input-set-key (kbd "s-H") 'dnixty/swap-windows-left)
  (exwm-input-set-key (kbd "s-J") 'dnixty/swap-windows-below)
  (exwm-input-set-key (kbd "s-K") 'dnixty/swap-windows-above)
  (exwm-input-set-key (kbd "s-L") 'dnixty/swap-windows-right))

;; The following can only apply to EXWM buffers, else it could have unexpected effects.
(push ?\s-  exwm-input-prefix-keys)
(define-key exwm-mode-map (kbd "s-SPC") #'exwm-floating-toggle-floating)

(exwm-input-set-key (kbd "s-i") #'follow-delete-other-windows-and-split)
(exwm-input-set-key (kbd "s-o") #'dnixty/toggle-single-window)
(exwm-input-set-key (kbd "s-O") #'exwm-layout-toggle-fullscreen)

(with-eval-after-load 'helm
  ;; Need `with-eval-after-load' here since 'helm-map is not defined in 'helm-config.
  (dnixty/define-keys helm-map
                        "s-\\" 'helm-toggle-resplit-and-swap-windows)
  (exwm-input-set-key (kbd "s-c") #'helm-resume)
  (exwm-input-set-key (kbd "s-b") #'helm-mini)
  (exwm-input-set-key (kbd "s-f") #'helm-find-files)
  (exwm-input-set-key (kbd "s-F") #'helm-locate)
  (when (fboundp 'dnixty/helm-locate-meta)
    (exwm-input-set-key (kbd "s-F") #'dnixty/helm-locate-meta))
  (exwm-input-set-key (kbd "s-g") 'dnixty/helm-grep-git-or-ag)
  (exwm-input-set-key (kbd "s-G") 'dnixty/helm-grep-git-all-or-ag)
  ;; Launcher
  (exwm-input-set-key (kbd "s-r") 'helm-run-external-command))

(require 'functions)
(exwm-input-set-key (kbd "s-<tab>") #'dnixty/switch-to-last-buffer)

;;; Emacs mode shortcuts.
(if (not (fboundp 'helm-org-switch))
    (exwm-input-set-key (kbd "s-t") (lambda () (interactive) (find-file (car org-agenda-files))))
  (exwm-input-set-key (kbd "s-t") #'helm-org-switch)
  (exwm-input-set-key (kbd "s-T") #'helm-org-switch-other-window))
(if (not (fboundp 'helm-eshell-switch))
    (exwm-input-set-key (kbd "s-<return>") #'eshell)
  (exwm-input-set-key (kbd "s-<return>") #'helm-eshell-switch)
  (exwm-input-set-key (kbd "S-s-<return>") #'helm-eshell-switch-other-window))
(when (fboundp 'magit-status)
  (exwm-input-set-key (kbd "s-v") #'magit-status))
(if (not (fboundp 'helm-elfeed-switch))
    (exwm-input-set-key (kbd "s-n") #'elfeed)
  (exwm-input-set-key (kbd "s-n") #'helm-elfeed-switch)
  (exwm-input-set-key (kbd "s-N") #'helm-elfeed-switch-other-window)) ; "n" for "news"
(if (not (fboundp 'helm-eww-switch))
    (exwm-input-set-key (kbd "s-e") #'eww)
  (exwm-input-set-key (kbd "s-e") #'helm-eww-switch)
  (exwm-input-set-key (kbd "s-E") #'helm-eww-switch-other-window))

(when (fboundp 'helm-pass)
  (exwm-input-set-key (kbd "s-p") #'helm-pass))

(autoload 'dnixty/slime-to-repl "lisp")
(exwm-input-set-key (kbd "s-<backspace>") #'dnixty/slime-switch-to-repl)
(defun dnixty/repl-switcher ()
  "Switch between Geiser and SLIME REPLs."
  (interactive)
  (pcase
      (completing-read "Lisp: " '(geiser slime))
    ("geiser"
     (autoload 'helm-geiser-repl-switch "scheme")
     (exwm-input-set-key (kbd "s-<backspace>") #'helm-geiser-repl-switch)
     (exwm-input-set-key (kbd "S-s-<backspace>") #'helm-geiser-repl-switch-other-window))
    ("slime"
     (exwm-input-set-key (kbd "s-<backspace>") #'dnixty/slime-switch-to-repl))))
(exwm-input-set-key (kbd "s-C-<backspace>") #'dnixty/repl-switcher)

;;; External application shortcuts.
(defun dnixty/exwm-start (command)
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))
(exwm-input-set-key (kbd "s-&") #'dnixty/exwm-start)

(when (require 'helm-exwm nil t)
  (add-to-list 'helm-source-names-using-follow "EXWM buffers")
  (setq helm-exwm-emacs-buffers-source (helm-exwm-build-emacs-buffers-source))
  (setq helm-exwm-source (helm-exwm-build-source))
  (setq helm-mini-default-sources `(helm-exwm-emacs-buffers-source
                                    helm-exwm-source
                                    helm-source-recentf
                                    ,(when (boundp 'helm-source-ls-git) 'helm-source-ls-git)
                                    helm-source-bookmarks
                                    helm-source-bookmark-set
                                    helm-source-buffer-not-found))
  (dnixty/define-keys
   helm-exwm-map
   "M-d" 'helm-buffer-run-kill-persistent
   "S-<return>" 'helm-buffer-switch-other-window)
  ;; Web browser
  (exwm-input-set-key (kbd "s-w") #'helm-exwm-switch-browser)
  (exwm-input-set-key (kbd "s-W") #'helm-exwm-switch-browser-other-window))

(when (require 'desktop-environment nil 'noerror)
  (setq desktop-environment-screenshot-directory "~/temp")
  (define-key desktop-environment-mode-map (kbd "s-z") #'desktop-environment-lock-screen)
  ;; Re-set s-l to navigate windows.
  (define-key desktop-environment-mode-map (kbd "s-l") #'windmove-right)
  (desktop-environment-mode))

(defun dnixty/suspend-to-sleep ()
  (interactive)
  (call-process "loginctl" nil nil nil "suspend"))
(exwm-input-set-key (kbd "s-Z") #'dnixty/suspend-to-sleep)

;;; Volume control
(when (require 'pulseaudio-control nil t)
  (exwm-input-set-key (kbd "s-<kp-subtract>") #'pulseaudio-control-decrease-volume)
  (exwm-input-set-key (kbd "s-<kp-add>") #'pulseaudio-control-increase-volume)
  (exwm-input-set-key (kbd "s-<kp-enter>") #'pulseaudio-control-toggle-current-sink-mute)
  (exwm-input-set-key (kbd "s--") #'pulseaudio-control-decrease-volume)
  (exwm-input-set-key (kbd "s-=") #'pulseaudio-control-increase-volume)
  (exwm-input-set-key (kbd "s-0") #'pulseaudio-control-toggle-current-sink-mute))

;;; Check for start-up errors. See ~/.profile.
(let ((error-logs (directory-files "~" t "errors.*log$")))
  (when error-logs
    (warn "Error during system startup.  See %s." (mapconcat 'identity error-logs ", "))
    (when (daemonp)
      ;; Non-daemon Emacs already brings up the *Warning* buffer.
      (setq initial-buffer-choice
            (lambda () (get-buffer "*Warnings*"))))))

;;; Some programs such as 'emacs' are better off being started in char-mode.
(defun dnixty/exwm-start-in-char-mode ()
  (when (string-prefix-p "emacs" exwm-instance-name)
    (exwm-input-release-keyboard (exwm--buffer->id (window-buffer)))))
(add-hook 'exwm-manage-finish-hook 'dnixty/exwm-start-in-char-mode)

;; Function to automatically toggle between internal/external screens.
(defun dnixty/exwm-change-screen-hook ()
  (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
        (xrandr-monitor-regexp "\n .* \\([^ \n]+\\)")
        default-output)
    (with-temp-buffer
      (call-process "xrandr" nil t nil)
      (goto-char (point-min))
      (re-search-forward xrandr-output-regexp nil 'noerror)
      (setq default-output (match-string 1))
      (forward-line)
      (if (not (re-search-forward xrandr-output-regexp nil 'noerror))
          (progn
            (call-process "xrandr" nil nil nil "--output" default-output "--auto" "--primary")
            (with-temp-buffer
              ;; Turn off all monitors that are not DEFAULT-OUTPUT.
              ;; See "--auto" in xrandr(1) and https://github.com/ch11ng/exwm/issues/529.
              (call-process "xrandr" nil t nil "--listactivemonitors")
              (goto-char (point-min))
              (while (not (eobp))
                (when (and (re-search-forward xrandr-monitor-regexp nil 'noerror)
                           (not (string= (match-string 1) default-output)))
                  (call-process "xrandr" nil nil nil "--output" (match-string 1) "--auto")))))
        (call-process
         "xrandr" nil nil nil
         "--output" (match-string 1) "--primary" "--auto"
         "--output" default-output "--off")
        (setq exwm-randr-workspace-monitor-plist (list 0 (match-string 1)))))))

(require 'exwm-randr)
(add-hook 'exwm-randr-screen-change-hook 'dnixty/exwm-change-screen-hook)
(exwm-randr-enable)

(provide 'init-exwm)

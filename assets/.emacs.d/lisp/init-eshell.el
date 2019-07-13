;;; Eshell

;;; Eshell gets initialized differently.  When eshell.el first gets loaded, only
;;; the core is defined and `eshell-load-hook' is called. For every Eshell
;;; session, `eshell-mode' is run: it resets `eshell-mode-map', it loads
;;; modules, runs their hooks and concludes with `eshell-first-time-mode-hook'
;;; (for the first session only) and `eshell-mode-hook'.

;; Emacs pinentry for GPG.
(require 'main)

;;; REVIEW: If history contains read-only text (e.g. accidental insertion of the prompt),
;;; `eshell-write-history' won't work.
;;; See #29153.

;;; REVIEW: Sometimes transmission-daemon does not start from Eshell.
;;; See #30465.

;;; REVIEW: Redirecting big output to file (e.g. /dev/null) is extremely slow.
;; > cat /usr/share/dict/british-english | wc -l
;;; The above line yields rancom results.  Plus it's much slower than
;; > wc -l /usr/share/dict/british-english
;;; while it should only cost an additional process to launch.
;;; See #29154.

;;; REVIEW: Cannot "C-c C-c" during a `sudo pacman -Syu`.  A bug was filed about that already.

;;; TODO: The buffer stutters when writing "in-place", e.g. pacman, git.
;;; It seems that it does not do it as much in `emacs -Q`.

;;; REVIEW: `eshell/sudo' should not read -u in command arguments.
;;; This fails: sudo pacman -Syu --noconfirm.
;;; http://www.gnu.org/software/emacs/manual/html_node/eshell/Built_002dins.html
;;; https://emacs.stackexchange.com/questions/5608/how-to-let-eshell-remember-sudo-password-for-two-minutes
;;; See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=27411
;;; and #28323.

;;; REVIEW: Eshell/Shell completion fails when PATH has a non-readable element.
;;; See https://github.com/emacs-helm/helm/issues/1785
;;; and https://debbugs.gnu.org/cgi/bugreport.cgi?bug=27300.

;;; REVIEW: 40M+ output: Stack overflow in regexp matcher
;;; See bug#28329.
;;; I guess the chunking is significant too.  Could you try saving
;;; the output chunks, with this:
;;
;; (defvar eshell-chunk-number 0)
;; (defconst eshell-output-chunk-dir "eshell-output")
;; (make-directory eshell-output-chunk-dir t)
;;
;; (defun catch-eshell-output-chunk ()
;;   (write-region eshell-last-output-block-begin
;;                 eshell-last-output-end
;;                 (format "%s/chunk.%d"
;;                         eshell-output-chunk-dir
;;                         eshell-chunk-number)
;;                 nil :quiet)
;;   (setq eshell-chunk-number (1+ eshell-chunk-number)))
;;
;; (add-hook 'eshell-output-filter-functions
;;           'catch-eshell-output-chunk)
;;;
;;; And then afterwards 'cat eshell-output/chunk.*' should hopefully
;;; reproduce it?


;;; REVIEW: Eshell mixes stderr and stdout it seems.
;;; Example:
;;; $ mu find --nocolor --sortfield=d --maxnum=500 flag:unread AND NOT flag:trashed >/dev/null
;;; mu: no matches for search expression (4)
;;; See #21605 "24.3; Eshell not using stderr".

;;; REVIEW: eshell/date does not support many options from GNU date.
;;; > date +%Z
;;; See #29157.
(when (< emacs-major-version 27)
  (require 'patch-eshell-26))

;;; TODO: Change face of eshell/* commands in commandline to avoid confusion
;;; with system programs.

;;; TODO: Merge/re-use documentation of eshell/* commands with their docstring.

;;; TODO: Hour is printed twice. We don't need to set this?
;; (setq eshell-ls-date-format (replace-regexp-in-string "^\\+*" "" (getenv "TIME_STYLE")))

;;; TODO: ls: Sort using locale.

;;; REVIEW: `kill -#' does not work.
;;; See #29156.

;;; Use TRAMP to use Eshell as root.
(require 'em-tramp)
(setq password-cache t)
(setq password-cache-expiry 3600)

(with-eval-after-load 'esh-module
  ;; REVIEW: It used to work, but now the early `provide' seems to backfire.
  (unless (boundp 'eshell-modules-list)
    (load "esh-module"))
  ;; Don't print the banner.
  (delq 'eshell-banner eshell-modules-list)
  (push 'eshell-tramp eshell-modules-list))

(setq
 eshell-ls-use-colors t
 ;; ffap-shell-prompt-regexp changes the behaviour of `helm-find-files' when
 ;; point is on prompt. I find this disturbing.
 ffap-shell-prompt-regexp nil
 eshell-history-size 1024
 eshell-hist-ignoredups t
 eshell-destroy-buffer-when-process-dies t)

(defun dnixty/eshell-prompt ()
  (let ((path (abbreviate-file-name (eshell/pwd))))
    (concat
     (when dnixty/eshell-status-p
       (propertize (or (dnixty/eshell-status-display) "") 'face font-lock-comment-face))
     (format
      (propertize "(%s@%s)" 'face '(:weight bold))
      (propertize (user-login-name) 'face '(:foreground "#33859e"))
      (propertize (system-name) 'face '(:foreground "#33859e")))
     (if (and (require 'magit nil t) (or (magit-get-current-branch) (magit-get-current-tag)))
         ;; We need "--show-prefix and not "--top-level" when we don't follow symlinks.
         (let* ((git-file-path (abbreviate-file-name (magit-rev-parse "--show-prefix")))
                prefix)
           (let ((common-folder (car (split-string git-file-path "/"))))
             (setq prefix (mapconcat 'identity (seq-take-while
                                                (lambda (s) (not (string= s common-folder)))
                                                (split-string path "/"))
                                     "/")))
           (format
            (propertize "[%s/%s@%s]" 'face '(:weight bold))
            (propertize prefix 'face `(:foreground ,(if (= (user-uid) 0) "#c23127" "#2aa889") :weight bold))
            (propertize (substring-no-properties path (min (length path) (1+ (length prefix))))
                        'face `(:foreground ,(if (= (user-uid) 0) "#d26937" "#edb443")))
            (or (magit-get-current-branch) (magit-get-current-tag))))
       (format
        (propertize "[%s]" 'face '(:weight bold))
        (propertize path 'face `(:foreground ,(if (= (user-uid) 0) "#c23127" "#2aa889") :weight bold))))
     (propertize "\n>" 'face '(:weight bold))
     " ")))

;;; Leave `eshell-highlight-prompt' to t as it sets the read-only property.
(setq eshell-prompt-function #'dnixty/eshell-prompt)
;;; If the prompt spans over multiple lines, the regexp should match
;;; last line only.
(setq-default eshell-prompt-regexp "^> ")

(with-eval-after-load 'em-term
  (dolist (p '("nix-shell" "pinentry-curses" "mpv"))
    (add-to-list 'eshell-visual-commands p)))
  ;; (setq eshell-visual-subcommands
        ;; Some Git commands use a pager by default.
        ;; Either invoke the subcommands in a term ("visual") or configure Git
        ;; to disable the pager globally.
        ;; '(("git" "log" "diff" "show")
        ;; '(("sudo" "wifi-menu") ; Arch Linux
         ;; ("sudo" "vi" "visudo"))))

;;; Alias management possibilities:
;;; - Version eshell-alias and store it in user-emacs-directory. Simplest and
;;; fastest, but aliases cannot be included conditionnaly, e.g. depending on the
;;; existence of a program.
;;; - Store eshell-alias in cache and populate it dynamically on startup.
;; (setq eshell-aliases-file (concat user-emacs-directory "eshell-alias"))
;;;
;;; `eshell/alias' is too slow as it reads and write the file on each definition.
;;; Let's write manually instead.
(with-eval-after-load 'em-alias
  ;;; If we read the alias list here, it means we make commandline-defined aliases persistent.
  ;; (eshell-read-aliases-list)
  (dolist
      (alias
       '(("l" "ls -1 $*")
         ("la" "ls -lAh $*")
         ("ll" "ls -lh $*")
         ;; TODO: Aliasing eshell/{cp,mv,ln} does not work.
         ;; REVIEW: Eshell/TRAMP's sudo does not work with aliases.
         ;; See #28320, #27168.
         ;; ("ls" "ls -F $*") ; not supported
         ;; ("emacs" "find-file $1")
         ;; ("cp" "eshell/cp -iv $*")
         ;; ("mv" "eshell/mv -iv $*")
         ("cpv" "cp -iv $*")
         ("mvv" "mv -iv $*")
         ("rmv" "rm -v $*")
         ("md" "eshell/mkdir -p $*")
         ("mkcd" "eshell/mkdir -p $* ; cd $1"))) ; TODO: '&&' does not work because mkdir exits with nil?
    (add-to-list 'eshell-command-aliases-list alias))
  (eshell-write-aliases-list))

;;; Hooks
;;; `nobreak-char-display' makes some output look weird, e.g. with 'tree'.
(add-hook 'eshell-mode-hook 'dnixty/turn-off-nobreak-char-display)

;;; History
;;; REVIEW: history: do not save failed Eshell commands (See `eshell-last-command-status')
;;; Eshell commands always return 0.
(setq eshell-input-filter
      (lambda (str)
        (not (or
              ;; Here we can filter out failing commands.  This is usually a bad
              ;; idea since a lot of useful commands have non-zero exit codes
              ;; (including Emacs/Eshell functions).
              ;; (/= eshell-last-command-status 0)
              (string= "" str)
              (string= "cd" str)
              (string-prefix-p "cd " str)
              ;; Filter out space-beginning commands from history.
              (string-prefix-p " " str)))))

;;; Shared history.
(defvar dnixty/eshell-history-global-ring nil
  "The history ring shared across Eshell sessions.")

(defun dnixty/eshell-hist-use-global-history ()
  "Make Eshell history shared across different sessions."
  (unless dnixty/eshell-history-global-ring
    (when eshell-history-file-name
      (eshell-read-history nil t))
    (setq dnixty/eshell-history-global-ring (or eshell-history-ring (make-ring eshell-history-size))))
  (setq eshell-history-ring dnixty/eshell-history-global-ring))
(add-hook 'eshell-mode-hook 'dnixty/eshell-hist-use-global-history)

(defun dnixty/eshell-history-remove-duplicates ()
  "Remove duplicates of last command in history.
This should be faster then `seq-uniq'."
  (let ((first (ring-ref eshell-history-ring 0))
        (index 1))
    (while (<= index (1- (ring-length eshell-history-ring)))
      (if (string= first (ring-ref eshell-history-ring index))
          ;; REVIEW: We could stop at the first match, it would be faster and it
          ;; would eliminate duplicates if we started from a fresh history.
          ;; From an existing history that would not clean up existing
          ;; duplicates beyond the first one.
          (ring-remove eshell-history-ring index)
        (setq index (1+ index))))))
(add-hook 'eshell-pre-command-hook 'dnixty/eshell-history-remove-duplicates)

;; Always save history
(add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

;;; Version and encrypt history.
;; TODO: The following makes EPA always prompt for recipients for some reason.  Bug?
;; (make-directory "~/personal/history/" :parents)
;; (setq eshell-history-file-name (expand-file-name "eshell.gpg" "~/personal/history"))
;; (defun dnixty/fix-local-epa-file-encrypt-to ()
;;   (unless (local-variable-p 'epa-file-encrypt-to (current-buffer))
;;     (make-local-variable 'epa-file-encrypt-to))
;;   (setq epa-file-encrypt-to "mail@dnixty.xyz"))
;; (add-hook 'eshell-mode-hook 'dnixty/fix-local-epa-file-encrypt-to)

;;; Spawning
(when (require 'patch-helm nil 'noerror)
  (helm-defswitcher
   "Eshell"
   (lambda (b)
     (with-current-buffer b
       (or
        (derived-mode-p 'eshell-mode)
        (derived-mode-p 'shell-mode))))
   (lambda (&optional name)
     ;; TODO: Could add an option "prefer-eshell" to choose between Eshell/Shell by default.
     (eshell 'new)
     (when name
       (rename-buffer (format "*eshell<%s>*" name) 'unique)))
   nil
   (helm-build-dummy-source
       "Create shell buffer"
     :action `(("Create shell buffer" .
                (lambda (candidate)
                  (let ((new-buffer (save-window-excursion (shell (format "*shell<%s>*" candidate))
                                                           (current-buffer))))
                    (switch-to-buffer new-buffer))))))))

;;; Auto-suggestion
(when (require 'esh-autosuggest nil t)
  (setq esh-autosuggest-delay 0.75)
  (add-hook 'eshell-mode-hook 'esh-autosuggest-mode)
  (define-key esh-autosuggest-active-map (kbd "<tab>") 'company-complete-selection)
  (when (require 'helm-config nil t)
    (define-key company-active-map (kbd "M-p") 'helm-eshell-history)))

;;; Extra execution information
(defvar dnixty/eshell-status-p t
  "If non-nil, display status before prompt.")
(defvar dnixty/eshell-status--last-command-time nil)
(make-variable-buffer-local 'dnixty/eshell-status--last-command-time)
(defvar dnixty/eshell-status-min-duration-before-display 1
  "If a command takes more time than this, display its duration.")

(defun dnixty/eshell-status-display ()
  (when dnixty/eshell-status--last-command-time
    (let ((duration (time-subtract (current-time) dnixty/eshell-status--last-command-time)))
      (setq dnixty/eshell-status--last-command-time nil)
      (when (> (time-to-seconds duration) dnixty/eshell-status-min-duration-before-display)
        (format "#[STATUS] End time %s, duration %.3fs\n"
                (format-time-string "%F %T" (current-time))
                (time-to-seconds duration))))))

(defun dnixty/eshell-status-record ()
  (setq dnixty/eshell-status--last-command-time (current-time)))

(add-hook 'eshell-pre-command-hook 'dnixty/eshell-status-record)

;;; Detach
(when (require 'package-eshell-detach nil t)
  (defun dnixty/eshell-detach-set-keys ()
    (define-key eshell-mode-map (kbd "C-c C-z") 'eshell-detach-stop)
    (define-key eshell-mode-map (kbd "S-<return>") 'eshell-detach-send-input)
    (define-key eshell-mode-map (kbd "C-<return>") 'eshell-detach-attach))
  (add-hook 'eshell-mode-hook 'dnixty/eshell-detach-set-keys))

;; Man
(when (string= (file-symlink-p (executable-find "man")) "mandoc")
  ;; Some systems like Void Linux use mandoc instead of man and do not know the
  ;; --nj, --nh flags.
  (defun dnixty/pcmpl-args-mandoc-man-function (name)
    (let ((process-environment process-environment))
      ;; Setting MANWIDTH to a high number makes most paragraphs fit on a single
      ;; line, reducing the number of false positives that result from lines
      ;; starting with `-' that aren't really options.
      (push "MANWIDTH=10000" process-environment)
      (pcmpl-args-process-file "man" "--" name)))
  (setq pcmpl-args-man-function 'dnixty/pcmpl-args-mandoc-man-function))

;; Completion
(when (require 'bash-completion nil t)
  (when (and (or (executable-find "fish")
                 ;; "fish" needs not be in PATH with guix.
                 (executable-find "guix"))
             (require 'fish-completion nil t))
    (setq fish-completion-fallback-on-bash-p t)
    (global-fish-completion-mode)))

;; Misc.
(defun dnixty/eshell-append-region-to-command-line (begin end)
  (interactive "r")
  (require 'subr-x)                     ; For `string-trim'.
  (save-mark-and-excursion
    (let ((string (buffer-substring-no-properties begin end)))
      (setq string (string-trim string))
      (setq string (concat string " "))
      (goto-char (point-max))
      (ignore-errors (cycle-spacing 0))
      (insert string))))

(provide 'init-eshell)

;;; Dired
;;; WARNING: This file is loaded unconditionally on startup.
;;; We cannot assume that current buffer is in dired-mode.

(dnixty/define-keys dired-mode-map
                      "C-c h" 'dnixty/dired-toggle-humansize
                      "<left>" 'dired-up-directory
                      "<right>" 'dired-find-file
                      "SPC" 'dired-mark
                      "<backspace>" 'dired-up-directory
                      "b" 'dired-up-directory)

(when (require 'dired+ nil t)
  (toggle-diredp-find-file-reuse-dir 1))

;;; On a GNU system, ls has the option to sort folders first.
(if (string-match "^gnu.*" (prin1-to-string system-type))
    (setq dired-listing-switches "--group-directories-first -lha")
  (setq dired-listing-switches "-lha"))

;;; Switches are set before the hook is called, so we need to reload dired. The
;;; dired-internal-noselect is a lower level function, so it is faster. WARNING:
;;; Not sure if it is equivalent though.
;; (dired dired-directory dired-listing-switches)
(defun dnixty/dired-set-listing-switches ()
  (dired-internal-noselect dired-directory dired-listing-switches))

(setq wdired-allow-to-change-permissions t)

;;; omit-mode needs to be started _after_ omit-files redefinition.
(require 'dired-x)
(setq dired-omit-files "^\\.")

(setq dired-guess-shell-alist-user
      (list
       '("\\.ogg$" "mpv")
       '("\\.\\(jpe?g\\|png\\|git\\)$" "sxiv")
       '("\\.\\(mkv\\|mpe?g\\|avi\\|mp4\\|ogm\\)$" "mpv")))

(defvar dnixty/dired-showing-humansize t "If dired is displaying humansize or not.")

(defun dnixty/dired-toggle-humansize ()
  "Toggle displaying humansize in dired."
  (interactive)
  (let ((switch-regexp "\\(\\`\\| \\)-\\([a-gi-zA-Z]*\\)\\(h\\)\\([^ ]*\\)")
        case-fold-search)
    (while (string-match switch-regexp dired-actual-switches)
      (if (and (equal (match-string 2 dired-actual-switches) "")
               (equal (match-string 4 dired-actual-switches) ""))
          (setq dired-actual-switches
                (replace-match "" t t dired-actual-switches))
        (setq dired-actual-switches
              (replace-match "" t t dired-actual-switches 3))))
    (if dnixty/dired-showing-humansize
        (setq dnixty/dired-showing-humansize nil)
      (progn
        (setq dired-actual-switches
              (concat dired-actual-switches
                      (if (string-match-p "\\`-[[:alnum:]]+\\'"
                                          dired-actual-switches)
                          "h" " -h")))
        (setq dnixty/dired-showing-humansize t))))
  (revert-buffer))

(dolist (fun '(dired-omit-mode dnixty/dired-set-listing-switches))
  (add-hook 'dired-mode-hook fun))

(when (require 'dired-du nil t)
  (setq dired-du-size-format t)
  ;; dired-du needs some adjustments with a custom TIME_STYLE.
  (when (getenv "TIME_STYLE")
    (let* ((yyyy "[0-9][0-9][0-9][0-9]")
           (HH:MM "[ 0-2][0-9][:.][0-5][0-9]")
           (seconds "[0-6][0-9]\\([.,][0-9]+\\)?")
           (iso-mm-dd "[01][0-9]-[0-3][0-9]")
           (zone "[-+][0-2][0-9][0-5][0-9]")
           (iso-time (concat HH:MM "\\(:" seconds "\\( ?" zone "\\)?\\)?"))
           (iso (concat "\\(\\(" yyyy "-\\)?" iso-mm-dd "[ T]" iso-time
                        "\\|" yyyy "-" iso-mm-dd "\\)")))
      (setq directory-listing-before-filename-regexp
            (concat "\\([0-9][BkKMGTPEZY]? "
                    "|" iso "|"
                    "\\) +")))))

(when (executable-find "sxiv")
  (setq image-dired-external-viewer "sxiv"))

(defun dnixty/image-dired-setup ()
  (add-hook 'window-configuration-change-hook 'image-dired-line-up-dynamic nil t))
(add-hook 'image-dired-thumbnail-mode-hook 'dnixty/image-dired-setup)

(provide 'init-dired)

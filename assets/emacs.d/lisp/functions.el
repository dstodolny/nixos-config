;;; Functions

;;; Notes on mark and region: to get a consistent behaviour regardless of
;;; Transient mode, check `(use-region-p)'. It will work as expected if
;;; transient. If not, it will always be true as soon as the mark has been set
;;; once; so you need to make sure the mark is set as you want beforehand (e.g.
;;; whole buffer, single line...). This is the behaviour of `sort-lines'.
;;;
;;; The clean way to get static region boundaries and fallback on buffer boundaries:
;;
;; (let (start end)
;;   (if (use-region-p)
;;       (setq start (region-beginning) end (region-end))
;;     (setq start (point-min) end (point-max)))
;;
;;; If several commands act on region and the region size/pos is susceptible to change:
;;
;; (let ((start (set-marker (make-marker) (if (use-region-p) (region-beginning) (point-min))))
;;       (end (set-marker (make-marker) (if (use-region-p) (region-end) (point-end)))))
;;
;;; For commands that only work on regions:
;;
;; (defun count-lines-region (start end)
;;   "Print number of lines and characters in the region."
;;   (interactive "r")
;;   ...

(defun dnixty/call-process-to-string (program &rest args)
  "Call PROGRAM with ARGS and return output.
See also `process-lines'."
  ;; Or equivalently:
  ;; (with-temp-buffer
  ;;   (apply 'process-file program nil t nil args)
  ;;   (buffer-string))
  (with-output-to-string
    (with-current-buffer standard-output
      (apply 'process-file program nil t nil args))))

(defun dnixty/define-keys (map key def &rest bindings)
  "Like `define-key' but allow for defining several bindings at once.
`KEY' must be acceptable for `kbd'."
  (while key
    (define-key map (kbd key) def)
    (setq key (pop bindings)
          def (pop bindings))))

;; TODO: Bind this to ediff control panel.
(defun dnixty/ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

(defun dnixty/escape-region (&optional regex to-string)
  "Escape double-quotes and backslashes.
This is useful for writing Elisp strings containing those
characters. The optional parameters let you control the replacement of REGEX for
TO-STRING."
  (interactive)
  (unless regex (setq regex "\\([\"\\\\]\\)"))
  (unless to-string (setq to-string "\\\\\\1"))
  (while (re-search-forward regex (if (use-region-p) (region-end) (point-max)) t)
    (replace-match to-string)))

(defun dnixty/prettify ()
  "(Un)tabify, indent and delete trailing whitespace.

Tabify if `indent-tabs-mode' is true, otherwise use spaces.
Work on buffer or region.

If `dnixty/prettify-inhibit-p' is non-nil, it does nothing.

Require `dnixty/tabify-leading'."
  (interactive)
  (unless dnixty/prettify-inhibit-p
    (let ((start (set-marker (make-marker) (if (use-region-p) (region-beginning) (point-min))))
          (end (set-marker (make-marker) (if (use-region-p) (region-end) (point-max)))))
      (if indent-tabs-mode
          (dnixty/tabify-leading)
        (untabify start end))
      (indent-region start end)
      (save-restriction
        (narrow-to-region start end)
        (delete-trailing-whitespace)))))

(defcustom dnixty/prettify-inhibit-p t
  "Do not run `dnixty/prettify' if non-nil.
As this is not friendly to foreign projects, `dnixty/prettify' should be run
selectively."
  :safe 'booleanp)

(defun dnixty/flyspell-and-whitespace-mode ()
  "Toggle `flyspell-mode' and `whitespace-mode'."
  (interactive)
  (if (derived-mode-p 'prog-mode)
      (flyspell-prog-mode)
    (flyspell-mode)
    (when flyspell-mode
      (flyspell-buffer)))
  (whitespace-mode 'toggle))
(global-set-key (kbd "<f9>") #'dnixty/flyspell-and-whitespace-mode)

;;; From https://www.reddit.com/r/emacs/comments/70bn7v/what_do_you_have_emacs_show_when_it_starts_up/.
;;; Supply a random fortune cookie as the *scratch* message.
(defun dnixty/fortune-scratch-message ()
  (interactive)
  (let ((fortune
         (when (executable-find "fortune")
           (with-temp-buffer
             (shell-command "fortune" t)
             (while (not (eobp))
               (insert ";; ")
               (forward-line))
             (delete-trailing-whitespace (point-min) (point-max))
             (concat (buffer-string) "\n")))))
    (if (called-interactively-p 'any)
        (insert fortune)
      fortune)))

(defun dnixty/global-set-keys (key def &rest bindings)
  "Like `global-set-key' but allow for defining several bindings at once.
`KEY' must be acceptable for `kbd'."
  (while key
    (global-set-key (kbd key) def)
    (setq key (pop bindings)
          def (pop bindings))))

(defun dnixty/image-display-external ()
  "Display original image at point using external viewer."
  (interactive)
  (let ((file (or (get-text-property (point) 'original-file-name)
                  (let ((image (get-text-property (point) 'display)))
                    (when image
                      (plist-get (cdr image) :file))))))
    (if (not file)
        (message "No original file name found")
      (start-process "image-dired-thumb-external" nil
                     image-dired-external-viewer (expand-file-name file)))))
(define-key image-map (kbd "S-<return>") 'dnixty/image-display-external)

(defun dnixty/current-minor-modes ()
  "Return the list of minor modes enabled in the current buffer."
  (interactive)
  (delq nil
        (mapcar (lambda (mode)
                  (if (and (boundp mode) (symbol-value mode))
                      mode))
                minor-mode-list)))

(defun dnixty/local-set-keys (key def &rest bindings)
  "Like `local-set-key' but allow for defining several bindings at once.
`KEY' must be acceptable for `kbd'."
  (while key
    (local-set-key (kbd key) def)
    (setq key (pop bindings)
          def (pop bindings))))

(defun dnixty/move-border-left (arg)
  "Move window border in a natural manner.
If this is a window with its right edge being the edge of the
screen, enlarge the window horizontally. If this is a window with
its left edge being the edge of the screen, shrink the window
horizontally. Otherwise, default to enlarging horizontally.\n
Enlarge/Shrink by ARG columns, or 5 if ARG is nil."
  (interactive "P")
  (if (= (count-windows) 2)
      (dnixty/move-border-left-or-right arg t)))
(global-set-key (kbd "M-(") 'dnixty/move-border-left)

(defun dnixty/move-border-left-or-right (arg dir-left)
  "Wrapper around ‘move-border-left’ and ‘move-border-right’.
ARG is the number of columns to move.
If DIR-LEFT is t, then move left, otherwise move right."
  (interactive)
  (unless arg (setq arg 5))
  (let ((left-edge (= (car (window-edges)) 0)))
    (if (or
         (and left-edge dir-left)
         (and (not left-edge) (not dir-left)))
        (shrink-window arg t)
      (enlarge-window arg t))))

(defun dnixty/move-border-right (arg)
  "See `move-border-left'."
  (interactive "P")
  (if (= (count-windows) 2)
      (dnixty/move-border-left-or-right arg nil)))
(global-set-key (kbd "M-)") 'dnixty/move-border-right)

(defun dnixty/reset-fill-column ()
  "Reset `fill-column' to its default value."
  (setq fill-column (default-value 'fill-column)))

(defun dnixty/sort-lines-unique (arg)
  "Remove trailing white space, then duplicate lines, then sort the result.
Do not fold case with \\[universal-argument] or non-nil ARG."
  (interactive "P")
  (let ((start (set-marker (make-marker) (if (use-region-p) (region-beginning) (point-min))))
        (end (set-marker (make-marker) (if (use-region-p) (region-end) (point-end)))))
    (let ((sort-fold-case (if arg nil t)))
      (delete-trailing-whitespace start end)
      (delete-duplicate-lines start end)
      (sort-lines nil start end))))

(defun dnixty/swap-windows (&optional w1 w2)
  "If 2 windows are up, swap them.
Else if W1 is a window, swap it with current window.
If W2 is a window too, swap both."
  (interactive)
  (unless (or (= 2 (count-windows))
              (windowp w1)
              (windowp w2))
    (error "Ambiguous window selection"))
  (let* ((w1 (or w1 (car (window-list))))
         (w2 (or w2
                 (if (eq w1 (car (window-list)))
                     (nth 1 (window-list))
                   (car (window-list)))))
         (b1 (window-buffer w1))
         (b2 (window-buffer w2))
         (s1 (window-start w1))
         (s2 (window-start w2)))
    (with-temp-buffer
      ;; Some buffers like EXWM buffers can only be in one live buffer at once.
      ;; Switch to a dummy buffer in w2 so that we don't display any buffer twice.
      (set-window-buffer w2 (current-buffer))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1))
    (set-window-start w1 s2)
    (set-window-start w2 s1))
  (select-window w1))
(global-set-key (kbd "C-x \\") 'swap-windows)

(defun dnixty/swap-windows-left ()
  "Swap current window with the window to the left."
  (interactive)
  (dnixty/swap-windows (window-in-direction 'left)))
(defun dnixty/swap-windows-below ()
  "Swap current window with the window below."
  (interactive)
  (dnixty/swap-windows (window-in-direction 'below)))
(defun dnixty/swap-windows-above ()
  "Swap current window with the window above."
  (interactive)
  (dnixty/swap-windows (window-in-direction 'above)))
(defun dnixty/swap-windows-right ()
  "Swap current window with the window to the right."
  (interactive)
  (dnixty/swap-windows (window-in-direction 'right)))

(defun dnixty/switch-to-last-buffer ()
  "Switch to last open buffer in current window."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun dnixty/tabify-leading ()
  "Call `tabify' on leading spaces only.
Works on whole buffer if region is unactive."
  (interactive)
  (require 'tabify) ; Need this to initialize `tabify-regexp'.
  (let ((tabify-regexp-old tabify-regexp) start end)
    (if (use-region-p)
        (setq start (region-beginning) end (region-end))
      (setq start (point-min) end (point-max)))
    (unwind-protect
        (progn
          (setq tabify-regexp "^\t* [ \t]+")
          (tabify start end))
      (setq tabify-regexp tabify-regexp-old))))

;; Inspired by https://github.com/abo-abo/oremacs.git.
(defun dnixty/test-emacs ()
  "Return nil on error, t on success so that it can be added to
`kill-emacs-query-functions'."
  (interactive)
  (if (not user-init-file)
      (progn
        (message "No init file")
        t)
    (let ((output
           (shell-command-to-string
            (format "emacs --batch --eval \"
 (condition-case e
    (progn
      (load \\\"%s\\\")
      (message \\\"-OK-\\\"))
  (error
   (message \\\"ERROR:\\\")
   (signal (car e) (cdr e))))\""
                    user-init-file))))
      (if (string-match "-OK-" output)
          (progn
            (when (called-interactively-p 'any)
              (message "All is well"))
            t)
        (switch-to-buffer-other-window "*init file error*")
        (erase-buffer)
        (insert output)
        (search-backward "ERROR:")
        nil))))
(add-hook 'kill-emacs-query-functions 'dnixty/test-emacs)

;;; TODO: Store window configurations in a buffer-name-indexed alist? Not
;;; sure that would ever be useful.
(defvar single-window--last-configuration nil "Last window configuration before calling `delete-other-windows'.")
(defun dnixty/toggle-single-window ()
  "Un-maximize current window.
If multiple windows are active, save window configuration and
delete other windows.  If only one window is active and a window
configuration was previously save, restore that configuration."
  (interactive)
  (if (= (count-windows) 1)
      (when single-window--last-configuration
        (set-window-configuration single-window--last-configuration))
    (setq single-window--last-configuration (current-window-configuration))
    (delete-other-windows)))

(defun dnixty/toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not.
Run it in each window you want to 'freeze', i.e. prevent Emacs
from acting on it."
  (interactive)
  (message
   (if (let ((window (get-buffer-window (current-buffer))))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))
(global-set-key (kbd "<pause>") 'dnixty/toggle-window-dedicated)

(defun dnixty/toggle-window-split ()
  "Switch between vertical and horizontal split.
It only works for frames with exactly two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))
(global-set-key (kbd "C-x C-\\") 'toggle-window-split)

(defun dnixty/toggle-word-delim ()
  "Make underscore part of the word syntax or not.
This does not interfere with `subword-mode'."
  (interactive)
  (if (equal (char-syntax ?_) "_")
      (progn
        (modify-syntax-entry ?_ "w")
        (message "_ is a not word delimiter"))
    (modify-syntax-entry ?_ "_")
    (message "_ is a word delimiter")))

(defun dnixty/unfill-paragraph ()
  "Paragraph at point is unwrapped on one single line."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun dnixty/unfill-region ()
  "Unfill all paragraphs found in current region.
Each paragraph stand on its line."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

(defun dnixty/youtube-dl-url (&optional url)
  "Run 'youtube-dl' over the URL.
If URL is nil, use URL at point."
  (interactive)
  (setq url (or url (thing-at-point-url-at-point)))
  (let ((eshell-buffer-name "*youtube-dl*")
        (directory (seq-find (lambda (dir)
                               (and (file-directory-p dir) (expand-file-name dir)))
                             '("~/temp" "~/Videos" "~/Downloads")
                             ".")))
    (eshell)
    (when (eshell-interactive-process)
      (eshell t))
    (eshell-interrupt-process)
    (insert (format " cd '%s' && youtube-dl " directory) url)
    (eshell-send-input)))

(defun dnixty/insert-current-date () (interactive)
       (insert (shell-command-to-string "echo -n $(date \"+%Y-%m-%d %H:%M\")")))

(provide 'functions)

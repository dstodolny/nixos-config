;;; TODO: Replace useless individual comments with a single global comment.

(defun dnixty/turn-on-column-number-mode ()
  "Unconditionally turn on `column-number-mode' for the current buffer."
  (set (make-variable-buffer-local 'column-number-mode) t))

(defun dnixty/turn-on-complete-filename ()
  "Unconditionally turn on `comint-dynamic-complete-filename' for the current buffer."
  (add-to-list 'completion-at-point-functions 'comint-dynamic-complete-filename t))

(defun dnixty/turn-on-delete-trailing-whitespace ()
  "Add the `delete-trailing-whitespace' function to `before-save-hook'.
This does not affect .csv files."
  (unless (or (string= (file-name-extension buffer-file-name) "csv")
              ;; REVIEW: Emacs 26.1 hangs when running delete-trailing-whitespace on image.
              (eq major-mode 'image-mode))
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)))

(defun dnixty/turn-off-delete-trailing-whitespace ()
  "Unconditionally remove the `delete-trailing-whitespace' function to `before-save-hook'."
  (remove-hook 'before-save-hook 'delete-trailing-whitespace t))

(defun dnixty/turn-on-prettify-before-save ()
  "Unconditionally add the `dnixty/prettify' function to `before-save-hook'."
  (add-hook 'before-save-hook 'dnixty/prettify nil t))

(defun dnixty/turn-off-prettify-before-save ()
  "Unconditionally remove the `dnixty/prettify' function to `before-save-hook'."
  (remove-hook 'before-save-hook 'dnixty/prettify t))

(defun dnixty/turn-off-indent-tabs ()
  "Unconditionally turn off tab indentation."
  (setq indent-tabs-mode nil))

(defun dnixty/turn-on-indent-tabs ()
  "Unconditionally turn on tab indentation."
  (setq indent-tabs-mode t))

(defun dnixty/turn-off-line-number-mode ()
  "Unconditionally turn off `line-number-mode' fur the current buffer.."
  (set (make-variable-buffer-local 'line-number-mode) nil))

(defun dnixty/turn-on-newline-paragraph ()
  "Unconditionally make of newlines the start of a paragraph."
  (set (make-local-variable 'paragraph-start) "
"))

(defun dnixty/turn-off-nobreak-char-display ()
  (set (make-local-variable 'nobreak-char-display) nil))

(defun dnixty/turn-on-skeleton-markers ()
  "Allow skeletons to make markers to ease field navigation."
  (require 'patch-skeletons)
  (add-hook 'skeleton-end-hook 'dnixty/skeleton-make-markers))

(defun dnixty/turn-on-tab-width-to-4 ()
  "Unconditionally set tab width to 4."
  (setq tab-width 4))

(defun dnixty/turn-on-tab-width-to-8 ()
  "Unconditionally set tab width to 8."
  (setq tab-width 8))

(provide 'hook-functions)

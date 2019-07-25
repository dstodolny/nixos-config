;;; REVIEW: Ignore dups in the entire ring, not just the last entry.
;;; Reported upstream, see #30466.
(defun dnixty/eshell-add-input-to-history (input)
  "Add the string INPUT to the history ring.
Input is entered into the input history ring, if the value of
variable `eshell-input-filter' returns non-nil when called on the
input."
  (require 'subr-x)
  ;; TODO: Report this trick, so that "ls" and "ls " don't both get added to the
  ;; history.  Doing this from eshell-expand-input-functions breaks Eshell
  ;; because the input is not expected to be modified.
  (setq input (string-trim-right input))
  (when (funcall eshell-input-filter input)
    (when eshell-hist-ignoredups
      (ring-remove eshell-history-ring
                   (ring-member eshell-history-ring input)))
    (eshell-put-history input))
  (setq eshell-save-history-index eshell-history-index)
  (setq eshell-history-index nil))
(advice-add 'eshell-add-input-to-history :override 'dnixty/eshell-add-input-to-history)

(provide 'patch-eshell-26)

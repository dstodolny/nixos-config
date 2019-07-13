(defun dnixty/skeleton-make-markers ()
  "Save last skeleton markers in a list.
Hook function for skeletons."
  (while skeleton-markers
    (set-marker (pop skeleton-markers) nil))
  (setq skeleton-markers
        (mapcar 'copy-marker (reverse skeleton-positions))))

(defvar skeleton-markers nil
  "Markers for locations saved in `skeleton-positions'.")

(defun dnixty/skeleton-previous-position ()
  "Move to previous skeleton placeholder.
See `skeleton-next-position'."
  (skeleton-next-position t))

(defun dnixty/skeleton-next-position (&optional reverse)
  "Move to next skeleton placeholder.
If REVERSE it t, move to previous placeholder."
  (interactive "P")
  (let ((positions (mapcar 'marker-position skeleton-markers))
        (comp (if reverse '< '<=))
        pos
        prev)
    (when positions
      (setq pos (pop positions))
      (while (and pos (funcall comp pos (point)))
        (setq prev pos)
        (setq pos (pop positions)))
      (cond
       ((and reverse prev) (goto-char prev))
       (reverse (goto-char (car (last skeleton-markers))))
       (pos (goto-char pos))
       (t (goto-char (car skeleton-markers)))))))

(provide 'patch-skeletons)

;; REVIEW: The following should be included in EMMS 5.2.

(defvar emms-browser--cache-hash nil)
(defun emms-browser-cache-thumbnail-async (dir size)
  (unless emms-browser--cache-hash
    (setq emms-browser--cache-hash (make-hash-table :test 'equal)))
  (let* ((key (cons dir size))
         (val (gethash key emms-browser--cache-hash)))
    (or val
        (puthash key (emms-browser-cache-thumbnail dir size)
                 emms-browser--cache-hash))))

(defun emms-browser-clear-cache-hash ()
  (interactive)
  (clrhash emms-browser--cache-hash))

(setq emms-browser-covers 'emms-browser-cache-thumbnail-async)

(provide 'patch-emms)

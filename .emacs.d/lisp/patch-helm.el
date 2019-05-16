;; TODO: Since Helm 3.1, we can pop up buffers in other window/frame.
(defun dnixty/helm-source-buffer-not-found (&optional mode new-fn)
  "See `helm-source-buffer-not-found'."
  (helm-build-dummy-source
      "Create buffer"
    :action `(("Create buffer (C-u choose mode)" .
               (lambda (candidate)
                 (if (not ,new-fn)
                     (let ((mjm (or (and helm-current-prefix-arg
                                         (intern-soft (helm-comp-read
                                                       "Major-mode: "
                                                       helm-buffers-favorite-modes)))
                                    ,mode
                                    (cl-loop for (r . m) in auto-mode-alist
                                             when (string-match r candidate)
                                             return m)))
                           (buffer (get-buffer-create candidate)))
                       (if mjm
                           (with-current-buffer buffer (funcall mjm))
                         (set-buffer-major-mode buffer))
                       (switch-to-buffer buffer))
                   (let ((new-buffer (save-window-excursion (funcall ',new-fn candidate)
                                                            (current-buffer))))
                     (switch-to-buffer new-buffer))))))))

(defmacro helm-deflister (name predicate new-fn &optional extra-sources)
  (let ((buffer-source-name (format "%s buffers" name)))
    (add-to-list 'helm-source-names-using-follow buffer-source-name)
    `(defun ,(intern (format "helm-%s-buffers" (downcase name))) ()
       (interactive)
       (helm
        :sources
        (list (helm-make-source ,(format "%s buffers" name) 'helm-source-buffers
                :buffer-list (lambda ()
                               (cl-loop for b in (buffer-list)
                                        when (funcall ,predicate b)
                                        collect (buffer-name b))))
              ,@(let ((mode (intern (format "%s-mode" (downcase name)))))
                  (unless (fboundp mode)
                    ;; For cased mode names like `Info-mode'.
                    (setq mode (intern (format "%s-mode" name))))
                  (when (fboundp mode)
                    `((dnixty/helm-source-buffer-not-found
                       ',mode
                       ,new-fn))))
              ,@extra-sources)
        :buffer ,(format "*helm-%s-buffers*" name)))))

(defmacro helm-defswitcher (name predicate new-fn &optional helm-lister &rest extra-sources)
  "Create an interactive buffer switcher for NAME.
When current buffer is not of NAME, switch to last NAME session if any.
Otherwise create a new one and switch to it.
If current buffer is of NAME, show the buffer list."
  (let ((switch (intern (format "helm-%s-switch" (downcase name)))))
    `(progn
       (helm-deflister ,name ,predicate ,new-fn ,extra-sources)
       (defun ,switch (&optional menu other-window)
         (interactive "P")
         (if (or menu (funcall ,predicate (current-buffer)))
             ;; TODO: Funcall the return value of helm-deflister.
             (funcall ',(or helm-lister
                            (intern (format "helm-%s-buffers" (downcase name)))))
           (let ((last (cl-loop for buffer in (buffer-list)
                                when (funcall ,predicate buffer)
                                return buffer)))
             (if last
                 (if (get-buffer-window last)
                     (select-window (get-buffer-window last))
                   (funcall (if other-window 'switch-to-buffer-other-window 'switch-to-buffer) last))
               ;; First buffer:
               (let ((first-buffer (save-window-excursion (funcall #',new-fn) (current-buffer))))
                 (funcall (if other-window 'switch-to-buffer-other-window 'switch-to-buffer)
                          first-buffer))))))
       (defun ,(intern (format "helm-%s-switch-other-window" (downcase name))) (&optional menu)
         (interactive "P")
         (funcall ',switch menu 'other-window)))))

(provide 'patch-helm)

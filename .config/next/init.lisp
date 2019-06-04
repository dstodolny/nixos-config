(in-package :next)

;; Use development platform port.
;; (setf (get-default 'port 'path)
      ;; (format nil "~a/common-lisp/next/ports/gtk-webkit/next-gtk-webkit"
              ;; (uiop:getenv "HOME")))

;; (setf (cdr (last (eval (get-default 'window 'search-engines))))
;;       '(("yt" . "https://www.youtube.com/results?search_query=~a")))
;; (nconc (eval (get-default 'window 'search-engines))
       ;; '(("yt" . "https://www.youtube.com/results?search_query=~a")))

(define-key (key "C-M-b") #'switch-buffer
  (key "C-M-w") #'make-window
  (key "C-M-d") #'delete-buffer
  (key "C-;") #'execute-command
  (key "M-g") #'go-anchor-new-buffer
  (key "C-M-C") #'kill)

(defun eval-in-emacs (&rest s-exps)
  "Evaluate S-EXPS with emacsclient."
  (let ((s-exps-string (cl-strings:replace-all
                        (write-to-string
                         `(progn ,@s-exps) :case :downcase)
                        ;; Discard the package prefix.
                        "next::" "")))
    (format *error-output* "Sending to Emacs:~%~a~%" s-exps-string)
    (uiop:run-program
     (list "emacsclient" "--eval" s-exps-string))))

(define-command org-capture ()
  "Org-capture current page."
  (with-result* ((url (buffer-get-url))
                 (title (buffer-get-title)))
    (eval-in-emacs
     `(org-link-set-parameters
       "next"
       :store (lambda ()
                (org-store-link-props
                 :type "next"
                 :link ,url
                 :description ,title)))
     `(org-capture))))

(define-key (key "C-M-o") #'org-capture)

;; (define-command youtube-dl-current-page ()
;;   "Download a video in the currently open buffer."
;;   (with-result (url (buffer-get-url))
;;     (eval-in-emacs
;;      (if (search "youtu" url)
;;          `(progn (youtube-dl ,url) (youtube-dl-list))
;;          `(ambrevar/youtube-dl-url ,url)))))
;; (define-key (key "C-M-c d") 'youtube-dl-current-page)

;; (define-command play-video-in-current-page ()
;;   "Play video in the currently open buffer."
;;   (with-result (url (buffer-get-url))
;;     (uiop:run-program (list "mpv" url))))
;; (define-key (key "C-M-c v") 'play-video-in-current-page)

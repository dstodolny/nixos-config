;;; Org mode

;;; TODO: org-import should be able to parse "|" in CSV files.

(define-key org-mode-map (kbd "C-c C-a") 'org-agenda)

(setq
 ;; Disable line splitting on M-RET.
 org-M-RET-may-split-line '((default))
 org-insert-heading-respect-content t
 org-enforce-todo-dependencies t
 org-deadline-warning-days 7
 org-agenda-default-appointment-duration 60
 org-agenda-columns-add-appointments-to-effort-sum t
 org-ellipsis " […]"
 org-adapt-indentation nil
 ;; Add keywords.
 org-todo-keywords '((sequence "TODO(t)" "REVIEW(r)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))
  ;; Priorities.
 org-priority-start-cycle-with-default nil
 org-default-priority 67
 ;; Org-mode aligns text.
 indent-tabs-mode nil
 org-cycle-separator-lines 0
 org-catch-invisible-edits 'smart)

;;; Agendas.
;; (add-to-list 'org-agenda-files "~/personal/gtd/inbox.org.gpg")
;; (load "~/personal/gtd/agenda-list.el" 'noerror)

(setq org-agenda-files '("~/personal/gtd/inbox.org.gpg"
                         "~/personal/gtd/projects.org.gpg"
                         "~/personal/gtd/tickler.org.gpg"))

(setq org-refile-targets '(("~/personal/gtd/projects.org.gpg" :maxlevel . 3)
                           ("~/personal/gtd/someday.org.gpg" :level . 1)
                           ("~/personal/gtd/tickler.org.gpg" :maxlevel . 2)))

(when (require 'patch-helm nil 'noerror)
  (helm-defswitcher
   "Org"
   (lambda (b)
     (when (buffer-file-name b)
       (member (file-truename (buffer-file-name b)) (mapcar #'file-truename org-agenda-files))))
   (lambda() (find-file (car org-agenda-files)))
   nil
   (helm-make-source "Org agenda files" 'helm-source-ffiles
     ;; Unclear why, but if we don't copy the list, the last element gets removed.
     :candidates (lambda () (copy-list org-agenda-files)))))

;;; Set PDF association in Org-mode (original is 'default).
(setcdr (assoc "\\.pdf\\'" org-file-apps) 'emacs)

;;; Hooks.
(dolist (fun '(;; turn-off-auto-fill
               dnixty/turn-off-indent-tabs))
  (add-hook 'org-mode-hook fun))

(when (require 'org-contacts nil t)
  (let ((contacts "~/personal/contacts/contacts.org.gpg"))
    (when (file-exists-p contacts)
      ;; When used to auto-complete e-mail addresses, the file is automatically
      ;; loaded.  The buffer usually need not be restored by a desktop session.
      (when (and desktop-save-mode
                 (string-match "\\\\|" desktop-files-not-to-save))
        (setq desktop-files-not-to-save
              (concat (substring desktop-files-not-to-save 0 -2) "\\|" (regexp-quote (expand-file-name contacts)) "\\)")))
      (setq org-contacts-files (list contacts)))))

(when (require 'org-bullets nil t)
  (add-hook 'org-mode-hook 'org-bullets-mode))

;; (add-to-list
;;  'org-capture-templates
;;  `("w" "Web link" entry (file+headline ,(car org-agenda-files) "Links")
;;    "* %?%a\n:SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))\n"))

(setq org-capture-templates '(("c" "capture [inbox]" entry
                               (file+headline "~/personal/gtd/inbox.org.gpg" "Tasks")
                               "* %i%?")
                              ("w" "Web link [inbox]" entry
                               (file+headline "~/personal/gtd/inbox.org.gpg" "Links")
                               "* %?%a\n:SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))\n")
                              ("t" "Tickler" entry
                               (file+headline "~/personal/gtd/tickler.org.gpg" "Tickler")
                               "* %i%? \n %U")))

(setq org-agenda-custom-commands
      '(("o" "At the office" tags-todo "@office")
        ("i" "On the Internet" tags-todo "@internet")
        ("E" "Errands" tags-todo "@errands")
        ("A" "Anywhere" tags-todo "@anywhere")
        ("h" "At home" tags-todo "@home")
        ("n" "On NixOs" tags-todo "@nixos")))

(provide 'init-org)
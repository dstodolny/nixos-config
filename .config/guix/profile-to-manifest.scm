;; Run with:
;;     guile -s FILE ~/.guix-profile

(use-modules (guix profiles)
             (ice-9 match)
             (ice-9 pretty-print))

(define (guix-manifest where)
  (sort (map (lambda (entry)
                     (let ((out (manifest-entry-output entry)))
                       (if (string= out "out")
                           (manifest-entry-name entry)
                           (format #f "~a:~a"
                                   (manifest-entry-name entry)
                                   (manifest-entry-output entry)))))
                   (manifest-entries (profile-manifest where)))
        string<?))

;; Thanks to Ivan Vilata-i-Balaguer for this:
(define (guix-commit)
  (let ((guix-manifest (profile-manifest (string-append (getenv "HOME") "/.config/guix/current"))))
    (match (assq 'source (manifest-entry-properties (car (manifest-entries guix-manifest))))
      (('source ('repository ('version 0) _ _
                             ('commit commit) _ ...))
       commit)
      (_ #f))))

(match (command-line)
  ((_ where)
   (format #t ";; commit: ~a\n" (guix-commit))
   (pretty-print
    `(specifications->manifest
      ',(guix-manifest where))))
  (_ (error "Please provide the path to a Guix profile.")))

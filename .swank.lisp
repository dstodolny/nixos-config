(setf swank:*globally-redirect-io* t)
;; The following is required by SBCL for Next.
(setf swank:*communication-style* :fd-handler)

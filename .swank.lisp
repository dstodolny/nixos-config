(setf swank:*globally-redirect-io* t)
;; The following is required by SBCL for Next.
#+sbcl (setf swank:*communication-style* :fd-handler)

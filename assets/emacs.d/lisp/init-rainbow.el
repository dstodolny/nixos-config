;; Rainbow delimiters

(when (require 'rainbow-delimiters nil t)
  ;; See https://yoo2080.wordpress.com/2013/09/08/living-with-rainbow-delimiters-mode/.
  ;; TODO: The color saturation metioned in the URL fails when running in daemon mode.
  ;; https://github.com/Fanael/rainbow-delimiters/issues/36
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#fe1717")
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#589cff")
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#f1fe52")
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#44ff4c")
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#83b2ff")
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#6161ff")
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#35ff35")
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#7ca8ff")
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#50fec1")
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error
                      :strike-through t))
(provide 'init-rainbow)

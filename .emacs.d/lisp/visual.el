;;; Visual

(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
(load-theme 'gotham t)

(set-face-attribute 'default nil
		    :font "DejaVu Sans Mono-12"
		    :weight 'normal
		    :width 'normal
		    :height 140)

(set-face-background 'mouse "#d3ebe9")

(provide 'visual)

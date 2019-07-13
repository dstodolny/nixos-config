;;; Lua

(defvaralias 'lua-indent-level 'tab-width)

(defun dnixty/lua-set-compiler ()
  (setq compile-command (concat lua-default-application " " (shell-quote-argument buffer-file-name))))

(add-hook 'lua-mode-hook 'dnixty/lua-set-compiler)

(provide 'init-lua)

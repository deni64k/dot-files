;;; forth.el ---
;; Copyright (c) 2009, Denis Sukhonin <d.sukhonin@gmail.com>

(autoload 'forth-mode "gforth.el")
(autoload 'forth-block-mode "gforth.el")
(add-to-list 'auto-mode-alist '("\\.fs$" . forth-mode))
(add-to-list 'auto-mode-alist '("\\.fb$" . forth-block-mode))

(setq forth-program-name "pfe")
(setq forth-compile-command "pfe")

(add-hook 'forth-mode-hook (function (lambda ()
  (setq forth-indent-level 4)
  (setq forth-minor-indent-level 2)
  (setq forth-hilight-level 3))))

;;; forth.el ends here

;;; ispell.el ---
;; Copyright (c) 2009, Denis Sukhonin <d.sukhonin@gmail.com>

(when (featurep 'flyspell)
  (set-default 'ispell-skip-html t)

  (setq ispell-dictionary "british")

  (defun turn-on-flyspell ()
    "Force flyspell-mode on using a positive arg. For use in hooks."
    (interactive)
    (flyspell-mode 1))

  (autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
  (add-hook 'message-mode-hook 'turn-on-flyspell)
  (add-hook 'text-mode-hook 'turn-on-flyspell)
  (add-hook 'nxml-mode-hook 'turn-on-flyspell)
  (add-hook 'texinfo-mode-hook 'turn-on-flyspell)
  (add-hook 'TeX-mode-hook 'turn-on-flyspell)

  (add-hook 'c-mode-common-hook 'flyspell-prog-mode)
  (add-hook 'lisp-mode-hook 'flyspell-prog-mode)
  (add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode))

;;; ispell.el ends here

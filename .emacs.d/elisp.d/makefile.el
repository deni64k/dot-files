;;; makefile.el ---
;; Copyright (c) 2009, Denis Sukhonin <d.sukhonin@gmail.com>

(defun negval/makefile-mode-hook ()
  (interactive)
  (setq show-trailing-whitespace t))
(add-hook 'makefile-mode-hook 'negval/makefile-mode-hook)

;;; makefile.el ends here

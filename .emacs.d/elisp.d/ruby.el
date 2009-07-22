(defun negval/ruby-mode-hook ()
  (ruby-electric-mode)
  (setq ruby-indent-level 2))

(add-hook 'ruby-mode-hook 'negval/ruby-mode-hook)

(defalias 'perl-mode 'cperl-mode) ; cperl mode is what we want

(defun negval/cperl-mode-hook ()
  (interactive)
  (eval-when-compile (require 'cperl-mode))
  (setq
   cperl-hairy t                  ; parse hairy perl constructs
   cperl-indent-level 2           ; indent with 2 positions
   cperl-invalid-face (quote off) ; don't show stupid underlines
   cperl-electric-keywords t))    ; complete keywords

(add-hook 'cperl-mode-hook 'negval/cperl-mode-hook)

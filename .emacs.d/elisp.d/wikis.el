;;; wikis.el ---

(require 'moinmoin-mode)
(add-hook 'outline-mode-hook 'turn-on-flyspell)
(add-hook 'outline-mode-hook (lambda nil (longlines-mode +1)))

;;; wikis.el ends here

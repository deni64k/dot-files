;;; yasnippet.el ---
;; yaSnippet - yet another snippet extension for Emacs
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")
(add-hook 'yas/after-exit-snippet-hook
          '(lambda ()
             (indent-region yas/snippet-beg yas/snippet-end)))

;; hook for automatic reloading of changed snippets
(defun negval/update-yasnippets-on-save ()
  (when (string-match "/.emacs.d/snippets" buffer-file-name)
    (yas/load-directory negval/yasnippet-dir)))
(add-hook 'after-save-hook 'negval/update-yasnippets-on-save)

;;; yasnippet.el ends here

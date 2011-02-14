;;; yasnippet.el ---
;; yaSnippet - yet another snippet extension for Emacs

;; hook for automatic reloading of changed snippets
(defun negval/update-yasnippets-on-save ()
  (when (string-match "/snippets" buffer-file-name)
    (yas/reload-all)))
(add-hook 'after-save-hook 'negval/update-yasnippets-on-save)

;;; yasnippet.el ends here

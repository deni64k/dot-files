(defun negval/browse-apropos-url (prefix prompt)
  (interactive)
  (let* ((thing (region-or-word-at-point)))
    (setq thing (read-string (format prompt thing) nil nil thing))
    (browse-apropos-url (concat prefix " " thing))))

(when (require-maybe 'inf-haskell)
  (setq auto-mode-alist
        (append auto-mode-alist
                '(("\\.[hg]s$"  . haskell-mode)
                  ("\\.hi$"     . haskell-mode)
                  ("\\.l[hg]s$" . literate-haskell-mode)))
        haskell-font-lock-symbols t)
  (autoload 'haskell-mode "haskell-mode"
    "Major mode for editing Haskell scripts." t)
  (autoload 'literate-haskell-mode "haskell-mode"
    "Major mode for editing literate Haskell scripts." t)
  (add-hook 'haskell-mode-hook
            (lambda nil
              (turn-on-haskell-decl-scan)
              (turn-on-haskell-doc-mode)
              (turn-on-haskell-indent)
              (turn-on-font-lock)
              (turn-on-eldoc-mode)))

  (add-hook 'haskell-mode-hook
            (lambda nil
              (local-set-key (kbd "C-c h") 'haskell-hoogle)
              (local-set-key (kbd "C-c C-h") 'haskell-hayoo))))

(negval/lock-comment-notes 'haskell-mode)

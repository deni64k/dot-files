(defun negval/emacs-lisp-mode-hook ()
  (interactive)
  ;; overrides the global f7 for compilation
  (local-set-key (kbd "<f7>") 'eval-buffer)
  (local-set-key (kbd "<C-f7>") 'eval-region)

  (setq lisp-indent-offset '+
        indent-tabs-mode t)
  (abbrev-mode 1)
  (set-fill-column 100)
  (auto-fill-mode 1)
  (local-set-key (kbd "RET") 'newline-and-indent))

;; show some functions as keywords
(font-lock-add-keywords 'emacs-lisp-mode
                        '(("\\<\\(quote\\|add-hook\\)" .
                           font-lock-keyword-face)))
;; recognize some things as functions
(font-lock-add-keywords 'emacs-lisp-mode
                        '(("\\<\\(set\\|setq\\|require-maybe\\|when-available\\|add-hook\\)\\>" .
                           font-lock-function-name-face)))
;; recognize some things as constants
(font-lock-add-keywords 'emacs-lisp-mode
                        '(("\\<\\(nil\\|\\t\\)\\_>" .
                           font-lock-constant-face)))

(add-hook 'emacs-lisp-mode-hook 'negval/emacs-lisp-mode-hook)

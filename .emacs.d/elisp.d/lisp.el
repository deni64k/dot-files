;;; lisp.el ---
;;; Copyright (c) 2010, Denis Sukhonin <d.sukhonin@gmail.com>

(add-hook 'lisp-mode-hook
          (lambda ()
            (setq lisp-indent-offset '+
                  indent-tabs-mode t)
            (turn-on-eldoc-mode)
            (abbrev-mode 1)
            (set-fill-column 100)
            (auto-fill-mode 1)
            (set (make-local-variable 'slime-lisp-implementations)
                 (list (assoc 'sbcl slime-lisp-implementations)))
            (local-set-key (kbd "RET") 'newline-and-indent)))

(when (require-maybe 'info-look)
  (info-lookup-add-help
   :mode 'lisp-mode
   :regexp "[^][()'\" \t\n]+"
   :ignore-case t
   :doc-spec '(("(ansicl)Symbol Index" nil nil nil))))

(require-maybe 'inf-lisp)

;;; lisp.el ends here

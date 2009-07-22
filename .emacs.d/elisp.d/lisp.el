;; Lisp
(add-to-list 'load-path "~/emacs.d/elisp/slime")

(add-hook 'lisp-mode-hook
          (lambda ()
            (setq lisp-indent-offset '+
                  indent-tabs-mode t)
            (abbrev-mode 1)
            (set-fill-column 100)
            (auto-fill-mode 1)
            (local-set-key (kbd "RET") 'newline-and-indent)))

(when (require-maybe 'info-look)
  (info-lookup-add-help
   :mode 'lisp-mode
   :regexp "[^][()'\" \t\n]+"
   :ignore-case t
   :doc-spec '(("(ansicl)Symbol Index" nil nil nil))))

(require-maybe 'inf-lisp)

;; SLIME
(when (require-maybe 'slime-autoloads)
  (slime-setup)
  (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
  (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
  (add-hook 'slime-load-hook (lambda () (require 'slime-autodoc)))
  (add-hook 'slime-mode-hook (lambda () (slime-autodoc-mode t)))

  (custom-set-variables
   '(slime-use-autodoc-mode t)
   '(slime-autodoc-use-multiline-p t)
   '(slime-autodoc-delay 0.4))

  (eval-after-load "slime"
    '(progn
       (slime-setup '(slime-fancy slime-asdf slime-banner slime-fuzzy
                                  slime-autodoc slime-repl slime-tramp))
       (setq inferior-lisp-program "sbcl"
             slime-complete-symbol*-fancy t
             slime-complete-symbol-function 'slime-fuzzy-complete-symbol
             slime-net-coding-system 'utf-8-unix)
       )))

;;; slime.el ---
;;; Copyright (c) 2010, Denis Sukhonin <d.sukhonin@gmail.com>

(require 'slime-autoloads)
(slime-setup)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
;(add-hook 'slime-load-hook (lambda () (require 'slime-autodoc)))
;(add-hook 'slime-mode-hook (lambda () (slime-autodoc-mode nil)))

(custom-set-variables
 '(slime-use-autodoc-mode nil)
 ;'(slime-autodoc-use-multiline-p t)
 ;'(slime-autodoc-delay 0.4)
 )

(eval-after-load "slime"
  '(progn
     (slime-setup '(slime-fancy slime-asdf slime-banner slime-fuzzy
                    slime-repl slime-tramp ; slime-autodoc
                    ))
     (setq inferior-lisp-program "sbcl --noinform --no-linedit"
           slime-complete-symbol*-fancy t
           slime-complete-symbol-function 'slime-fuzzy-complete-symbol
           slime-net-coding-system 'utf-8-unix)
     (setq slime-lisp-implementations
           (list `(clojure ,(swank-clojure-cmd) :coding-system utf-8-unix :init swank-clojure-init)
                 '(sbcl ("sbcl") :coding-system utf-8-unix)))
     (custom-set-variables
      '(slime-lisp-host "localhost"))
     ))

;;; slime.el ends here

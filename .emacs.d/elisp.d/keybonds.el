;; keybonds
(global-set-key [(hyper a)] 'mark-whole-buffer)
(global-set-key [(hyper v)] 'yank)
(global-set-key [(hyper c)] 'kill-ring-save)
(global-set-key [(hyper x)] 'kill-region)
(global-set-key [(hyper s)] 'save-buffer)
(global-set-key [(hyper l)] 'goto-line)
(global-set-key [(hyper w)]
                (lambda () (interactive) (delete-window)))
(global-set-key [(hyper z)] 'undo)
(global-set-key [(hyper f)] 'isearch-forward)
(global-set-key [(hyper o)] 'projectile-find-file)
(global-set-key [(shift hyper f)] 'isearch-backward)

(global-set-key (kbd "H-1") (lambda() (interactive) (find-file "~/Code/me")))
(global-set-key (kbd "H-2") (lambda() (interactive) (find-file "~/Code/pagefair")))
(global-set-key (kbd "H-3") (lambda() (interactive) (find-file "~/Code/go/src/github.com/housinganywhere/ha")))
(global-set-key (kbd "H-0") (lambda() (interactive) (find-file "~/.emacs.d/init.el")))

;; go-mode
(when-declared 'go-mode-map
  (define-key go-mode-map (kbd "C-x , f") 'go-test-current-file)
  (define-key go-mode-map (kbd "C-x , t") 'go-test-current-test)
  (define-key go-mode-map (kbd "C-x , p") 'go-test-current-project)
  (define-key go-mode-map (kbd "C-x , b") 'go-test-current-benchmark)
  (define-key go-mode-map (kbd "C-x , x") 'go-run)
  (define-key go-mode-map (kbd "C-x d") 'godoc)
  (define-key go-mode-map (kbd "C-x l") 'golint)
  (define-key go-mode-map (kbd "C-c .") 'ac-complete-go))

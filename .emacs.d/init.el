(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comment-style (quote multi-line))
 '(custom-safe-themes
   (quote
    ("28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "3ed645b3c08080a43a2a15e5768b893c27f6a02ca3282576e3bc09f3d9fa3aaa" default)))
 '(safe-local-variable-values
   (quote
    ((eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
	   (add-hook
	    (quote write-contents-functions)
	    (lambda nil
	      (delete-trailing-whitespace)
	      nil))
	   (require
	    (quote whitespace))
	   "Sometimes the mode needs to be toggled off and on."
	   (whitespace-mode 0)
	   (whitespace-mode 1))
     (whitespace-line-column . 80)
     (whitespace-style face tabs trailing lines-tail)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(blink-cursor-mode 0)

(require 'package)
(package-initialize)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; setup el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

;; sass
(el-get-bundle haml-mode)
(el-get-bundle sass-mode)

;; markdown
(el-get-bundle markdown-mode)
(el-get-bundle markdown-preview-mode)

;; yaml
(el-get-bundle yaml-mode)

;; python
(el-get-bundle python-mode)
(el-get-bundle pyenv)

;; quickly find files in a git, mercurial or other repository
(el-get-bundle projectile)
(projectile-global-mode)
(setq projectile-indexing-method 'native)

;; Keybonds
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

(global-set-key (kbd "H-1") (lambda() (interactive) (find-file "~/Code/go/src/github.com/housinganywhere/ha")))
(global-set-key (kbd "H-0") (lambda() (interactive) (find-file "~/.emacs.d/init.el")))

;; mac switch meta key
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)

;; disable tab indentation by default
(el-get-bundle dtrt-indent)
(setq tab-width 2)
(setq standard-indent 2)
(setq-default indent-tabs-mode nil)

;; sublimify emacs
;;
;; auto reload buffer
;; don't annoy after git stash and checkouts
(global-auto-revert-mode t)

;; highlight words on click
(defun click-select-word (event)
  (interactive "e")
  (hi-lock-mode 0)
  (let* ((selection (buffer-substring-no-properties (region-beginning) (region-end)))
	 (phrase (concat "\\b" (regexp-quote selection) "\\b")))
    (message "a %s" selection)
    (message "b %s" (regexp-quote selection))
    (message "c %s" phrase)
    (highlight-regexp phrase)))

;(global-set-key [mouse-1] nil)
;(global-set-key [down-mouse-1] 'click-select-word)

;; overwrite highlighted text
(delete-selection-mode 1)

;; disable useless keybinds
; (global-set-key [S-mouse-1] 'mouse-set-region)

;; dired tunes
(el-get-bundle dired-single)

;; ensure environment variables inside Emacs look the same as in the user's shell
(el-get-bundle exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; load theme
(when (display-graphic-p)
  (add-to-list 'load-path "~/.emacs.d/themes")
  ; (require 'blackboard-theme)
  (require 'feng-shui-theme)
  (el-get-bundle diff-hl)
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

;; manage backups
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 2
   kept-old-versions 2
   version-control t)

;; desktop mode
(desktop-save-mode 1)

;; autocomplete
(el-get-bundle auto-complete)
(setq ac-auto-show-menu nil)
(setq ac-use-quick-help t)
(setq ac-auto-start nil)
;; (define-key ac-completing-map [return] nil)
;; (define-key ac-completing-map "\r" nil)
;; (define-key ac-completing-map [down] nil)
;; (define-key ac-completing-map (kbd "C-n") nil)
(add-to-list 'ac-user-dictionary "d.sukhonin@gmail.com")
(add-to-list 'ac-user-dictionary "housinganywhere")
(add-to-list 'ac-user-dictionary "HousingAnywhere")

;; editorconfig.org
(el-get-bundle editorconfig)
(editorconfig-mode 1)

;; snippets
(el-get-bundle yasnippet)
(el-get-bundle yasnippet-snippets)
(el-get-bundle yasnippets)
(el-get-bundle auto-complete-yasnippet)
; (yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

(el-get-bundle icicles)
(el-get-bundle icicles-cmd1)
(el-get-bundle icicles-cmd2)
(el-get-bundle icicles-face)
(el-get-bundle icicles-fn)
(el-get-bundle icicles-install)
(el-get-bundle icicles-mac)
(el-get-bundle icicles-mcmd)
(el-get-bundle icicles-mode)
(el-get-bundle icicles-opt)
(el-get-bundle icicles-var)
(icy-mode 1)

;; react
(el-get-bundle web-mode)
(add-to-list 'load-path "~/.emacs.d")
(require 'jsx-mode)
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (or (equal web-mode-content-type "jsx")
	  (equal web-mode-content-type "js"))
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))
(setq js-indent-level 2)
(setq jsx-indent-level 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
(add-hook 'jsx-mode-hook
          (lambda () (auto-complete-mode 1)))

;; flycheck
(el-get-bundle flycheck)
(require 'flycheck)
(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."
  :command ("jsxhint" source)
  :error-patterns
  ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes (web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "jsx")
              ;; enable flycheck
              (flycheck-select-checker 'jsxhint-checker)
              (flycheck-mode))))

;; go-mode
;; (exec-path-from-shell-copy-env "GOROOT")
(exec-path-from-shell-copy-env "GOPATH")
(setenv "PATH" (concat
		;; (file-name-as-directory (getenv "GOROOT")) "bin"
		;; ":"
		(file-name-as-directory (getenv "GOPATH")) "bin"
		":"
		(getenv "PATH")))

;; (setq el-get-go (concat (file-name-as-directory (getenv "GOROOT")) "bin/go"))
;; (setq go-command (concat (file-name-as-directory (getenv "GOROOT")) "bin/go"))
(setq el-get-go "/usr/local/bin/go")
(setq go-command "/usr/local/bin/go")
(setq go-eldoc-gocode (concat (file-name-as-directory (getenv "GOPATH")) "bin/gocode"))

(el-get-bundle go-mode)
(el-get-bundle go-def)
(el-get-bundle go-imports)
(el-get-bundle go-lint)
(el-get-bundle go-eldoc)
;; (el-get-bundle go-test)
(el-get-bundle go-autocomplete)
(el-get-bundle go-errcheck-el)
(require 'go-mode-autoloads)
(add-hook 'before-save-hook #'gofmt-before-save)
(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(set-face-attribute 'eldoc-highlight-function-argument nil
                    :underline nil :foreground "red"
                    :weight 'bold)
(define-key go-mode-map (kbd "C-x , f") 'go-test-current-file)
(define-key go-mode-map (kbd "C-x , t") 'go-test-current-test)
(define-key go-mode-map (kbd "C-x , p") 'go-test-current-project)
(define-key go-mode-map (kbd "C-x , b") 'go-test-current-benchmark)
(define-key go-mode-map (kbd "C-x , x") 'go-run)
(define-key go-mode-map (kbd "C-x d") 'godoc)
(define-key go-mode-map (kbd "C-x l") 'golint)
(define-key go-mode-map (kbd "C-c .") 'ac-complete-go)
(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)
(require 'go-errcheck)

(el-get-bundle rust-mode)
(el-get-bundle rustfmt)
(add-hook 'rust-mode-hook #'rustfmt-enable-on-save)

;; interpret ansi colors in shell output
(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

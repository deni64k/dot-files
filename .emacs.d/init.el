
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comment-style (quote multi-line))
 '(custom-safe-themes
   (quote
    ("28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "3ed645b3c08080a43a2a15e5768b893c27f6a02ca3282576e3bc09f3d9fa3aaa" default)))
 '(rust-format-on-save (quote t))
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

;; http://www.emacswiki.org/cgi-bin/wiki/LocateLibrary
(defmacro require-soft (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if `require' fails."
  `(require ,feature ,file 'noerror))
(defmacro when-available (func &rest foo)
  "*Do something if FUNCTION is available."
  (declare (indent defun))
  `(when (fboundp ,func) ,@foo))
(defmacro when-declared (var &rest foo)
  "*Do something if VARIABLE is declared."
  (declare (indent defun))
  `(when (boundp ,var) ,@foo))

(defun load-user-file (file)
  "Load a user configuration file from ~/.emacs.d/elisp.d"
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file "~/.emacs.d/elisp.d")))

(add-to-list 'load-path "~/.emacs.d/vendor")

(load-user-file "general.el")
(load-user-file "package.el")
(load-user-file "appearance.el")
(load-user-file "helm.el")

;; quickly find files in a git, mercurial or other repository
(el-get-bundle projectile)
(projectile-global-mode)
(setq projectile-indexing-method 'native)

(load-user-file "programming.el")

(load-user-file "doxygen.el")

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

;; disable useless keybinds
; (global-set-key [S-mouse-1] 'mouse-set-region)

;; dired tunes
(el-get-bundle dired-single)

;; ensure environment variables inside Emacs look the same as in the user's shell
(el-get-bundle exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

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

;; snippets
; (el-get-bundle yasnippet)
; (el-get-bundle yasnippet-snippets)
; (el-get-bundle yasnippets)
; (el-get-bundle auto-complete-yasnippet)
;; (yas-reload-all)
; (add-hook 'prog-mode-hook #'yas-minor-mode)

;(el-get-bundle icicles)
;(el-get-bundle icicles-cmd1)
;(el-get-bundle icicles-cmd2)
;(el-get-bundle icicles-face)
;(el-get-bundle icicles-fn)
;(el-get-bundle icicles-install)
;(el-get-bundle icicles-mac)
;(el-get-bundle icicles-mcmd)
;(el-get-bundle icicles-mode)
;(el-get-bundle icicles-opt)
;(el-get-bundle icicles-var)
;(icy-mode 1)

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
;; (el-get-bundle flycheck)
;; (require 'flycheck)
;; (flycheck-define-checker jsxhint-checker
;;   "A JSX syntax and style checker based on JSXHint."
;;   :command ("jsxhint" source)
;;   :error-patterns
;;   ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
;;   :modes (web-mode))
;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (when (equal web-mode-content-type "jsx")
;;               ;; enable flycheck
;;               (flycheck-select-checker 'jsxhint-checker)
;;               (flycheck-mode))))

(load-user-file "go.el")

;; rust stuff

(el-get-bundle rust-mode)
(el-get-bundle rustfmt)

;; haskell stuff
(el-get-bundle haskell-mode)

(load-user-file "cxx.el")

(load-user-file "keybonds.el")

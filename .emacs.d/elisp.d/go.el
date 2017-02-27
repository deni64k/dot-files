;; go stuff
(if (executable-find "go")
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
  (setq el-get-go (executable-find "go"))
  (setq go-command (executable-find "go"))
  (setq go-eldoc-gocode (concat (file-name-as-directory (getenv "GOPATH")) "bin/gocode"))
  (el-get-bundle go-mode)
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
  (require 'go-autocomplete)
  (require 'auto-complete-config)
  (ac-config-default)
  (require 'go-errcheck)
)

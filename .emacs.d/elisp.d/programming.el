;; automatic indentation style detection
(el-get-bundle dtrt-indent)

;; editorconfig.org
(el-get-bundle editorconfig)
(editorconfig-mode 1)

;; sass
(el-get-bundle haml-mode)
(el-get-bundle sass-mode)

;; markdown
(el-get-bundle markdown-mode)
(el-get-bundle markdown-preview-mode)

;; yaml
(el-get-bundle yaml-mode)
(add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode))  ;; salt-stack files

;; python
(el-get-bundle python-mode)
(el-get-bundle pyenv)
(add-to-list 'auto-mode-alist '("WORKSPACE\\'" . python-mode))
(add-to-list 'auto-mode-alist '("BUILD\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.bzl\\'" . python-mode))

;; auto completion
(el-get-bundle company-mode)
(add-hook 'after-init-hook 'global-company-mode)


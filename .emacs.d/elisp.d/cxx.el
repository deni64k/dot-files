;; ;; helm-gtags
;; (el-get-bundle ggtags)
;; (el-get-bundle helm-gtags)

;; (setq
;;  helm-gtags-ignore-case t
;;  helm-gtags-auto-update t
;;  helm-gtags-use-input-at-cursor t
;;  helm-gtags-pulse-at-cursor t
;;  helm-gtags-prefix-key "\C-cg"
;;  helm-gtags-suggested-key-mapping t)

;; (require 'helm-gtags)
;; ;; Enable helm-gtags-mode
;; (add-hook 'dired-mode-hook 'helm-gtags-mode)
;; (add-hook 'eshell-mode-hook 'helm-gtags-mode)
;; (add-hook 'c-mode-hook 'helm-gtags-mode)
;; (add-hook 'c++-mode-hook 'helm-gtags-mode)
;; (add-hook 'asm-mode-hook 'helm-gtags-mode)

;; (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
;; (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
;; (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
;; (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
;; (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
;; (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

;; c++ stuff
;; (require 'ggtags)
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;;               (ggtags-mode 1))))

;; (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
;; (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
;; (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
;; (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
;; (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
;; (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

;; (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

;; (el-get-bundle company-mode)
;; (require 'company)
;; (add-hook 'after-init-hook 'global-company-mode)
;; (setq company-backends (delete 'company-semantic company-backends))
;; ;(define-key c-mode-base-map   (kbd "<tab>") 'company-complete)
;; ;(define-key c++-mode-base-map (kbd "<tab>") 'company-complete)

;; (el-get-bundle company-c-headers)
;; (add-to-list 'company-backends 'company-c-headers)
;; (setq company-c-headers-path-system (list "/usr/include/c++/4.2.1"))

;; (require 'cc-mode)
;; (require 'semantic)
;; (global-semanticdb-minor-mode 1)
;; (global-semantic-idle-scheduler-mode 1)
;; (semantic-mode 1)
;; (semantic-add-system-include "/usr/include/c++/4.2.1" 'c++-mode)

(defconst negval/c-style
  '((c-tab-always-indent . t)
    (c-basic-offset . 2)
    ;; (ident-tabs-mode . t)
    (c-offsets-alist
     (brace-list-open . +)
     (brace-list-intro . +)
     (brace-list-entry . -)
     (case-label . +)
     (inline-open . 0)
     (namespace-open . 0)
     (namespace-close . 0)
     (innamespace . 0)
     (statement-cont . +)
     (substatement-open . 0)
     (topmost-intro-cont . 0)
     )))

(c-add-style "negval" negval/c-style)
;; (c-set-style "negval" nil)

;; (c-set-offset 'innamespace 0)
(setq auto-mode-alist (append (list '("\\.h\\'" . c++-mode)) auto-mode-alist))

(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'cc-mode-common-hook 'google-set-c-style)

(require 'modern-cpp-font-lock)
(modern-c++-font-lock-global-mode t)

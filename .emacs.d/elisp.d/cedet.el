;;; cedet.el ---
;; Copyright (c) 2009, Denis Sukhonin <d.sukhonin@gmail.com>

;; http://xtalk.msk.su/~ott/ru/writings/emacs-devenv/EmacsCedet.html
;;
;; Enabling Semantic (code-parsing, smart completion) features.
(load-file "~/.emacs.d/elisp/cedet/common/cedet.el")
(setq semantic-load-turn-useful-things-on t)
(semantic-load-enable-excessive-code-helpers)
;;(semantic-load-enable-semantic-debugging-helpers)

(setq senator-minor-mode-name "SN"
      semantic-imenu-auto-rebuild-directory-indexes nil)

(global-srecode-minor-mode 1)
(global-semantic-mru-bookmark-mode 1)
(global-semantic-folding-mode 1)      ; hide and show blocks
(global-semantic-idle-tag-highlight-mode 1)

(require 'semantic-decorate-include)
(require 'semantic-ia)
(require 'semantic-tag)

(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(setq-mode-local c++-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(setq-mode-local erlang-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
;;(setq-mode-local c++-mode semanticdb-implied-include-tags
;;                 (semantic-find-tags-included "/home/dsuhonin/projects/idfs/include/pch.hxx"))

;; Enable SemanticDB.
(require 'semanticdb)
;; TODO: parse `gcc -print-prog-name=cc1plus` -v < /dev/null
(require 'semantic-gcc)
(semantic-gcc-setup)
(global-semanticdb-minor-mode 1)
(setq semanticdb-default-save-directory "~/.tmp/semanticdb")
(when (not (file-exists-p semanticdb-default-save-directory))
  (make-directory semanticdb-default-save-directory))
(dolist (dir '("/usr/include" "/usr/local/include" "/usr/include/postgresql"
               "/usr/include/c++/4.2" "/usr/include/c++/4.2/backwards"
               "/usr/include/c++/4.3" "/usr/include/c++/4.3/backwards"))
  (when (file-exists-p dir)
    (semantic-add-system-include dir 'c-mode)
    (semantic-add-system-include dir 'c++-mode)))
(setq boost-dir (find-if #'file-exists-p '("/usr/include/boost"
                                           "/usr/local/include/boost")))
(when boost-dir
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file
               (concat boost-dir "/boost/config.hpp")))

;; EAssist.
(when (require-maybe 'eassist)
  (setq eassist-header-switches
        (append eassist-header-switches
                '(("hxx" "cxx") ("cxx" "hxx")
                  ("H" "cc") ("cc" "H"))
                )))

;; SpeedBar.
(require 'semantic-sb)

;; EDE.
(require 'semantic-lex-spp)
(global-ede-mode 1)

;; Enable Imenu.
(add-hook 'semantic-init-hooks
  (lambda ()
    (imenu-add-to-menubar "TAGS")))

(defun negval/cedet-hook nil
  (local-set-key (kbd "C-<return>") 'semantic-ia-complete-symbol)
  (local-set-key (kbd "C-c ?") 'semantic-ia-complete-symbol-menu)
  (local-set-key (kbd "C-c >") 'semantic-complete-analyze-inline)
  (local-set-key (kbd "C-c =") 'semantic-decoration-include-visit)
  (local-set-key (kbd "C-c p") 'semantic-analyze-proto-impl-toggle)
  (local-set-key (kbd "C-c j") 'semantic-ia-fast-jump)
  (local-set-key (kbd "C-c q") 'semantic-ia-show-doc)
  (local-set-key (kbd "C-c s") 'semantic-ia-show-summary)
  (local-set-key (kbd "C-c h") 'eassist-switch-h-cpp))

(add-hook 'c-mode-common-hook 'negval/cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'negval/cedet-hook)
(add-hook 'erlang-mode-hook 'negval/cedet-hook)

;; gnu global support
;; (require 'semanticdb-global)
;; (semanticdb-enable-gnu-global-databases 'c-mode)
;; (semanticdb-enable-gnu-global-databases 'c++-mode)

;; ctags
;; (require 'semanticdb-ectag)
;; (semantic-load-enable-primary-exuberent-ctags-support)

(custom-set-variables
 '(semantic-idle-scheduler-work-idle-time 10)
 '(semantic-self-insert-show-completion-function
   (lambda nil (semantic-ia-complete-symbol-menu (point))))
 '(global-semantic-tag-folding-mode
   t nil (semantic-util-modes)))

;;; cedet.el ends here

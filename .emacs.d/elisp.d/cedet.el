;;; cedet.el ---
;; Copyright (c) 2009, Denis Sukhonin <d.sukhonin@gmail.com>

;; http://xtalk.msk.su/~ott/ru/writings/emacs-devenv/EmacsCedet.html
;;
;; Enabling Semantic (code-parsing, smart completion) features.

(remove-if (lambda (path) (string-match "/cedet" path)) load-path)
(add-to-list 'load-path "~/.emacs.d/elisp/cedet")
(load-file "~/.emacs.d/elisp/cedet/common/cedet.el")

(semantic-load-enable-excessive-code-helpers)
;;(semantic-load-enable-semantic-debugging-helpers)

(setq senator-minor-mode-name "SN"
      semantic-imenu-auto-rebuild-directory-indexes nil)

(global-srecode-minor-mode 1)
(global-semantic-mru-bookmark-mode 1)
(global-semantic-folding-mode 1)      ; hide and show blocks

(require 'semantic-decorate-include)
(require 'semantic-ia)
(require 'semantic-tag)

;; Enable SemanticDB.
(require 'semanticdb)
(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(local project unloaded system recursive))
(setq-mode-local c++-mode semanticdb-find-default-throttle
                 '(local project unloaded system recursive omniscience))
(setq-mode-local erlang-mode semanticdb-find-default-throttle
                 '(local project unloaded system recursive))
;;(setq-mode-local c++-mode semanticdb-implied-include-tags
;;                 (semantic-find-tags-included "/home/dsuhonin/projects/idfs/include/pch.hxx"))

(require 'semantic-gcc)
(semantic-gcc-setup)
(global-semanticdb-minor-mode 1)
(setq semanticdb-default-save-directory "~/.tmp/semanticdb")
(when (not (file-exists-p semanticdb-default-save-directory))
  (make-directory semanticdb-default-save-directory))
(dolist (dir '("/usr/local/include"       ; for *BSD systems
               "/usr/include/postgresql"  ; for Debian's pgsql
               ))
  (when (file-exists-p dir)
    (semantic-add-system-include dir 'c-mode)
    (semantic-add-system-include dir 'c++-mode)))
(setq boost-dir (find-if #'file-exists-p '("/usr/include/boost"
                                           "/usr/local/include/boost")))
(when boost-dir
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file
               (concat boost-dir "/config.hpp")))

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
(require 'semanticdb-global)
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)

;; ctags
(require 'semanticdb-ectag)
(add-to-list 'semantic-ectag-program-list "exctags") ; for *BSD
(semantic-load-enable-primary-exuberent-ctags-support)

(custom-set-variables
 '(semantic-idle-scheduler-mode t)
 '(semantic-idle-scheduler-idle-time 20)
 '(semantic-idle-scheduler-work-idle-time 30)
 '(semantic-self-insert-show-completion-function
   (lambda nil (semantic-ia-complete-symbol-menu (point))))
 '(global-semantic-tag-folding-mode
   t nil (semantic-util-modes)))

;;; cedet.el ends here

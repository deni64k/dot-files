;;; org.el ---

(custom-set-variables
 '(org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "STARTED(s)" "|" "DONE(d)"
                                 "CANCELED(c)")))
 '(org-special-ctrl-a/e t)
 '(mark-diary-entries-in-calendar t)
 '(diary-file "~/documents/org/diary")
 '(org-agenda-files (list "~/documents/org/TODO.org"
                          "~/documents/org/programming.org"))
 '(org-default-notes-file "~/documents/org/notes.org")
 '(org-directory "~/documents/org")
 '(org-insert-mode-line-in-empty-file t))

(require 'org-install)
(require 'org)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'file-coding-system-alist (cons "\\.org$" 'utf-8))

(add-hook 'org-mode-hook 'turn-on-font-lock)

;; diary setup
(require 'diary-lib)
(add-hook 'diary-display-hook 'fancy-diary-display)

;; link abbrevs
(add-to-list 'org-link-abbrev-alist '("emacswiki" . "http://www.emacswiki.org/cgi-bin/wiki/"))
(add-to-list 'org-link-abbrev-alist '("google" . "http://www.google.com/search?q="))

;; remember mode
(org-remember-insinuate)
(define-key global-map (kbd "C-c r") 'org-remember)

(setq org-remember-templates
      '(
        ("Todo" ?t "* TODO %?\n %i\n %a" (concat org-directory "/TODO.org") "Tasks")
        ))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(setq org-log-done t)

;;; org.el ends here

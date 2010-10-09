(require 'ruby-mode)
(require 'ruby-electric)

(defun negval/ruby-mode-hook ()
  (ruby-electric-mode 1)
  (setq ruby-indent-level 2)
  (local-set-key (kbd "RET") 'ruby-reindent-then-newline-and-indent))

(add-hook 'ruby-mode-hook 'negval/ruby-mode-hook)

(add-to-auto-mode-alist 'ruby-mode '("\\.rb\\'"
				     "Capfile\\'"
				     "Rakefile\\'"
				     "\\.rake\\'"
				     "Gemfile\\'"
				     "\\.gemspec\\'"))

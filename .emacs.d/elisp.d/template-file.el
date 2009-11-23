;;; template-file.el ---

;; Copyright (c) 2009, Denis Sukhonin <d.sukhonin@gmail.com>

(require 'file-template)

(custom-set-variables
 '(file-template-insert-automatically t)
 '(file-template-paths '("~/.emacs.d/insert/"))
 '(file-template-mapping-alist
   '(
     ("\\.lisp$" . "template.lisp")
     ("\\.asd$" . "template.asd")
     ("\\.el$" . "template.el")
     ("\\.c$" . "template.c")
     ("\\.h$" . "template.h")
     ("\\.\\(cxx\\|cc\\|C\\|cpp\\)$" . "template.cxx")
     ("\\.\\(hxx\\|hh\\|H\\|hpp\\)$" . "template.hxx")
     ("[Mm]akefile" . "template.mk")
     ("\\.sh$" . "template.sh")
     ("\\.csh$" . "template.csh")
     ("\\.zsh$" . "template.zsh")
     ("\\.php$" . "template.php")
     ("\\.pl$" . "template.pl")
     ("\\.py$" . "template.py")
     ("\\.rb$" . "template.rb")
     )))

(autoload 'file-template-auto-insert "file-template" nil t)
(autoload 'file-template-find-file-not-found-hook "file-template" nil t)
(add-hook 'find-file-not-found-hooks 'file-template-find-file-not-found-hook 'append)

;;; template-file.el ends here

;;; el-get.el ---
;;; Copyright (c) 2011, Denis Sukhonin <d.sukhonin@gmail.com>

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; So the idea is that you copy/paste this code into your *scratch* buffer,
;; hit C-j, and you have a working el-get.
(if (require 'el-get nil t)
    (message "el-get is already installed, try M-x el-get-update")
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(setq el-get-sources
      `(el-get color-theme
               nav
               doxymacs
               cmake-mode
               cedet
               (:name ecb
                :type cvs
                :module "ecb"
                :url ":pserver:anonymous@ecb.cvs.sourceforge.net:/cvsroot/ecb"
                :build ,(list (concat  "make CEDET=" " EMACS=" el-get-emacs)))
               rvm
               (:name ruby-mode
                :type svn
                :url "http://svn.ruby-lang.org/repos/ruby/trunk/misc")
               flymake-ruby
               rinari rhtml-mode sass-mode haml-mode
               coffee-mode
               haskell-mode
               distel
               go-mode
               rainbow-mode
               slime clojure-mode swank-clojure
               magit
               yasnippet
               emacs-w3m
               ))
(el-get 'sync)

;;; el-get.el ends here

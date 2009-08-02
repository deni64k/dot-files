(defun negval/lock-comment-notes (mode)
  "Highlight note keywords in comments fot mode MODE."
  (font-lock-add-keywords mode
                          '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):"
                             1 font-lock-warning-face prepend))))

(mapc (lambda (mode) (negval/lock-comment-notes mode))
      '(
        c-common-mode
        cmake-mode
        cperl-mode
        emacs-lisp-mode
        erlang-mode
        haskell-mode
        html-mode
        lisp-mode
        ruby-mode
        scheme-mode
        ))

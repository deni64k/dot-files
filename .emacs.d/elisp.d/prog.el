(defun negval/lock-comment-notes (mode)
  "Highlight note keywords in comments fot mode MODE."
  (font-lock-add-keywords mode
                          '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):"
                             1 font-lock-warning-face prepend))))

(mapc (lambda (mode) (negval/lock-comment-notes mode))
      '(
        c-common-mode
        cperl-mode
        haskell-mode
        lisp-mode
        ruby-mode
        scheme-mode
        ))

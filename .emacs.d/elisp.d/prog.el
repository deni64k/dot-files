(defun negval/lock-comment-notes (mode)
  "Highlight note keywords in comments fot mode MODE."
  (font-lock-add-keywords mode
                          '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):"
                             1 font-lock-warning-face prepend))))

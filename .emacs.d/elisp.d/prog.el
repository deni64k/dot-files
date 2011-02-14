(defun negval/lock-comment-notes (mode)
  "Highlight note keywords in comments fot mode MODE."
  (font-lock-add-keywords mode
                          '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\|KLUDGE\\):"
                             1 font-lock-warning-face prepend))))

(mapc (lambda (mode) (negval/lock-comment-notes mode))
      '(c-common-mode
        cmake-mode
        cperl-mode
        clojure-mode
        clojure-test-mode
        emacs-lisp-mode
        erlang-mode
        forth-mode
        haskell-mode
        html-mode
        lisp-mode
        ruby-mode
        scheme-mode
        ))

;; compilation; if compilation is successful, autoclose the compilation win
;; http://www.emacswiki.org/cgi-bin/wiki/ModeCompile
(defun compile-autoclose (buffer string)
  (cond ((and (string-match "finished" string)
              (not (string-match "warning" string)))
         (bury-buffer "*compilation*")
         (winner-undo)
         (message "Build successful."))
        (t
         (message "Compilation exited abnormally: %s" string))))
(setq compilation-finish-functions 'compile-autoclose)
(setq compilation-window-height 12)

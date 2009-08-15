;;; text.el ---
;; Copyright (c) 2009, Denis Sukhonin <d.sukhonin@gmail.com>

(defun negval/text-mode-hook ()
  (interactive)
  (set-fill-column 78)                    ; lines are 78 chars long ...
  (auto-fill-mode t)                      ; ... and wrapped around automagically

  ;; http://taiyaki.org/elisp/word-count/src/word-count.el
  (when (require-maybe 'word-count) ; count the words
    (word-count-mode t))

  (when (require-maybe 'filladapt) ; do the intelligent wrapping of lines,...
    (filladapt-mode t))) ; ... (bullets, numbering) if
                                        ; available
(add-hook 'text-mode-hook 'negval/text-mode-hook)

;; turn on autofill for all text-related modes
(toggle-text-mode-auto-fill)

;;; text.el ends here

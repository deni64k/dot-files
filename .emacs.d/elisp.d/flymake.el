;;; flymake.el ---
;; Copyright (c) 2009, Denis Sukhonin <d.sukhonin@gmail.com>

(when (require-maybe 'flymake)
  (push '("\\.cxx\\'" flymake-simple-make-init) flymake-allowed-file-name-masks)
  (push '("\\.cc\\'"  flymake-simple-make-init) flymake-allowed-file-name-masks)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (flymake-can-syntax-check-file buffer-file-name)
                (local-set-key (kbd "C-c C-v")
                               (lambda ()
                                 (interactive)
                                 (flymake-mode t)
                                 (flymake-goto-next-error)))
                (local-set-key (kbd "C-c v")
                               (lambda ()
                                 (interactive)
                                 (flymake-mode t)
                                 (flymake-goto-next-error)
                                 (flymake-display-err-menu-for-current-line))))))
  )

;;; flymake.el ends here

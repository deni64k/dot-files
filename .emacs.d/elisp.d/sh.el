;;; sh.el ---
;; Copyright (c) 2009, Denis Sukhonin <d.sukhonin@gmail.com>

;; Use ANSI colors within shell-mode.
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; syntax check
(defun sh-check-finish-hook (buf msg)
  "Function, that is executed at the end of sh check"
  (when (not (string-match "finished" msg))
    (next-error 1 t)))

(define-compilation-mode sh-check-mode "SH"
  "Mode for check sh source code."
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-scroll-output) nil)
  (set (make-local-variable 'compilation-finish-functions)
       (list 'sh-check-finish-hook)))

(defun sh-check-syntax ()
  "Check syntax of current file"
  (interactive)
  (when (string-match "^\\(ba\\|z\\)sh" (symbol-name sh-shell))
    (save-some-buffers t)
    (compilation-start (concat (symbol-name sh-shell) " -n " (buffer-file-name))
                       'sh-check-mode)))

(add-hook 'shell-script-mode-hook
          (lambda nil (local-set-key (kbd "C-c l") 'sh-check-syntax)))

;;; sh.el ends here

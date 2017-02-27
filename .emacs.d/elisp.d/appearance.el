;; load theme
(when (display-graphic-p)
  (push (substitute-in-file-name "~/.emacs.d/themes") custom-theme-load-path)
  (load-theme 'borland-blue t)
  ;; (load-theme 'blackboard t)

  (set-default-font "Menlo-11")

  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(mouse ((t (:background "lawn green")))))

  (el-get-bundle diff-hl)
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(blink-cursor-mode 0)

;; interpret ansi colors in shell output
(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

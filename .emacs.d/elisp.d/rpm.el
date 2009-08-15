;;; rpm.el ---
;; Copyright (c) 2009, Denis Sukhonin <d.sukhonin@gmail.com>

;; rpm-spec-mode
(autoload 'rpm-spec-mode "rpm-spec-mode.el" "RPM spec mode." t)
(setq auto-mode-alist (append '(("\\.spec" . rpm-spec-mode))
                              auto-mode-alist))

;;; rpm.el ends here

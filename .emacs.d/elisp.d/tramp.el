;;; tramp.el ---
;; Copyright (c) 2009, Denis Sukhonin <d.sukhonin@gmail.com>

(require 'tramp)

(when +unix-p+
  (setq tramp-default-method "ssh"))
(when +win32-p+
  (setq tramp-default-method "plink"))

;;; tramp.el ends here

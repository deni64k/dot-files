;;; git.el ---
;; Copyright (c) 2009, Denis Sukhonin <d.sukhonin@gmail.com>

(when (require-maybe 'magit)
  (custom-set-faces
   '(magit-item-highlight ((((class color) (background dark))
                            (:background "gray10"))))
   '(magit-log-tag-label ((((class color) (background dark))
                           (:background "gray15" :foreground "goldenrod1"))))
   '(magit-log-head-label ((((class color) (background dark))
                            (:background "gray15" :foreground "green yellow")))))
  (global-set-key (kbd "C-c m") 'magit-status))

;;; git.el ends here

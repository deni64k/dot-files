;;; saveplace.el ---
;;; Copyright (c) 2009, Denis Sukhonin <d.sukhonin@gmail.com>

;; SavePlace: this puts the cursor in the last place you editted
;; a particular file. This is very useful for large files.
(when (require-maybe 'saveplace)
  (progn
    (setq-default save-place t)))

;;; saveplace.el ends here

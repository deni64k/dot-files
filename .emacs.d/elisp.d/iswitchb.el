;;; iswitchb.el ---
;;; Copyright (c) 2009, Denis Sukhonin <d.sukhonin@gmail.com>

;; iswitchb: helps me efficiently to switch buffers.
(when-available 'iswitchb-mode
  (progn
    (defun iswitchb-local-keys ()
      (mapc (lambda (K)
              (let* ((key (car K)) (fun (cdr K)))
                (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
            '(("<right>" . iswitchb-next-match)
              ("<left>"  . iswitchb-prev-match)
              ("<up>"    . ignore             )
              ("<down>"  . ignore             ))))
    (add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)
    (setq iswitchb-default-method 'samewindow)
    (iswitchb-mode t)               ; buffer switching; easier than icicles
    (require-maybe 'iswitchb-highlight)
    ))

;;; iswitchb.el ends here

;;; use tw-it.el

(defun twitter ()
  (interactive)
  (when (require-maybe 'twit)
    (custom-set-variables '(twit-user "neglectedvalue")
                          `(twit-pass ,(negval/put-secret twit-user))
                          '(twit-protocol "http")
                          '(twit-show-user-images t)
                          '(twit-user-image-dir "~/.tmp/twitter"))
    (custom-set-faces
     '(twit-message-face
       ((((class color) (background dark)) (:background "Gray8" :foreground "#ffffff"
                                            :family "Droid Sans Mono" :height 1.15))))
     '(twit-author-face
       ((((class color) (background dark)) (:foreground "Green Yellow"
                                            :family "Droid Sans" :height 0.9))))
     '(twit-info-face
       ((((class color) (background dark)) (:foreground "Dark Sea Green"
                                            :family "Droid Sans" :height 0.9))))
     '(twit-title-face
       ((((class color) (background dark)) (:background "Gray20" :foreground "DodgerBlue1"
                                            :box nil
                                            :family "Droid Sans Mono" :height 1.5 :weight bold))))
     '(twit-url-face ((t (:height 0.8 :weight normal)))))
    (make-directory twit-user-image-dir t)

    (twit-show-recent-tweets)
    (twit-follow-recent-tweets)
    ))

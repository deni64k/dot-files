;;; gnus.el ---
;;; Copyright (c) 2009, Denis Sukhonin <d.sukhonin@gmail.com>

(require 'gnus-setup)
(require 'gnus-art)
(require 'gnus-cite)

(require 'starttls)
(require 'smtpmail)

(setq user-mail-address (or (getenv "EMAIL") "d.sukhonin@gmail.com")
      user-full-name (or (getenv "FULLNAME") "Denis Sukhonin"))

;; (setq gnus-select-method '(nntp "reader.usenet4all.se"))
(setq gnus-select-method '(nntp "news.gmane.org")
      gnus-large-newsgroup 500)

;; Changing modeline to include also the date of the message
(setq gnus-summary-line-format "%U%R%z%I%(%[%4L: %-23,23f%]%) %s -- %d\n")

(setq gnus-sum-thread-tree-vertical        "│"
      gnus-sum-thread-tree-leaf-with-other "├─► "
      gnus-sum-thread-tree-single-leaf     "╰─► ")

;; (add-to-list 'gnus-secondary-select-methods
;;              '(nntp "news.gnus.org"))
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "gmail"
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "d.sukhonin@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; Tree view for groups.  I like the organisational feel this has.
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Threads!  I hate reading un-threaded email -- especially mailing
;; lists.  This helps a ton!
(setq gnus-summary-thread-gathering-function
      'gnus-gather-threads-by-subject)

;; Also, I prefer to see only the top level message.  If a message has
;; several replies or is part of a thread, only show the first
;; message.  'gnus-thread-ignore-subject' will ignore the subject and
;; look at 'In-Reply-To:' and 'References:' headers.
(setq gnus-thread-hide-subtree   t
      gnus-thread-ignore-subject t)

;;; gnus.el ends here

(defun jabber ()
  "Load jabber mode."
  (interactive)

  (defmacro jabber-account (jid &rest args)
    (let ((result (list `(symbol-name ',jid))))
      (unless (assoc :password args)
        (push `(cons :password (negval/put-secret ,jid)) result))
      (while args
        (let ((arg (pop args)))
          (cond
           ((eq arg :password)
            (push `(cons :password ,(pop args)) result))
           ((eq arg :connection-type)
            (push `(cons :connection-type ',(pop args)) result))
           ((eq arg :disabled)
            (push `(cons :disabled ,(pop args)) result))
           (t
            (error "Unknown symbol %s" (symbol-name ',arg)))
           ))
        )
      `(reverse (list ,@result))
      ))

  (require-maybe 'jabber-util)
  (require-maybe 'jabber-autoloads)

  (custom-set-variables
   ;; accounts
   '(jabber-account-list
     (list
      (jabber-account dsuhonin@jabber.mfisoft.ru :connection-type starttls)
      (jabber-account l5k@jabber.cc :connection-type starttls)))
   ;; defaults
   '(jabber-default-priority 50)
   '(jabber-default-show "")
   ;; roster
   '(jabber-roster-subscription-display '(("none" . " ")
                                          ("from" . "←")
                                          ("to"   . "→")
                                          ("both" . "⇄")))
   '(jabber-roster-line-format " %c %-25n %u %-8s %S")
   '(jabber-show-offline-contacts nil)
   '(jabber-roster-show-bindings nil)
   ;; autoaway
   '(jabber-autoaway-method 'jabber-termatime-get-idle-time)
   '(jabeer-autoaway-timeout 5)
   '(jabber-autoaway-status "I'm away")
   '(jabber-autoaway-priority 40))

  ;; (custom-set-faces
  ;;  '(jabber-roster-user-chatty
  ;;    ((((class color) (background dark)) (:background "#151515" :height 1.15))
  ;;     (((class color) (background light)) (:height 1.15))) t)
  ;;  '(jabber-roster-user-online
  ;;    ((((class color) (background dark)) (:background "#151515" :height 1.15))
  ;;     (((class color) (background light)) (:height 1.15))) t)
  ;;  '(jabber-roster-user-away
  ;;    ((((class color) (background dark)) (:background "#0a0a0a" :height 1.15))
  ;;     (((class color) (background light)) (:height 1.15))) t)
  ;;  '(jabber-roster-user-dnd
  ;;    ((((class color) (background dark)) (:background "#201515" :height 1.15))
  ;;     (((class color) (background light)) (:height 1.15))) t)
  ;;  '(jabber-roster-user-xa
  ;;    ((((class color)) (:height 0.9))) t)
  ;;  '(jabber-roster-user-offline
  ;;    ((((class color)) (:height 0.9))) t)
  ;;  '(jabber-roster-user-error
  ;;    ((((class color)) (:height 0.9))) t)
  ;;  '(jabber-activity-face
  ;;    ((((class color)) (:foreground "firebrick1"))) t)
  ;;  '(jabber-activity-personal-face
  ;;    ((((class color)) (:foreground "DodgerBlue1"))) t)
  ;;  )
  ;; )
  )
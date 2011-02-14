;;; helpers.el ---
;; Copyright (c) 2009, Denis Sukhonin <d.sukhonin@gmail.com>

(require 'macro-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require-maybe  (http://www.emacswiki.org/cgi-bin/wiki/LocateLibrary)
;; this is useful when this .emacs is used in an env where not all of the
;; other stuff is available
(defmacro require-maybe (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if `require' fails."
  `(require ,feature ,file 'noerror))

(defmacro when-available (func &rest foo)
  "*Do something if FUNCTION is available."
  (declare (indent defun))
  `(when (fboundp ,func) ,@foo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load passwords
(defmacro negval/put-secret (for-what)
  `(progn
     (require 'secrets "~/.secrets.gpg")
     (let ((secret (cdr (assoc ',for-what negval/*secrets*))))
       (unless secret
         (error "Couldn't find password for %s" (symbol-name ',for-what)))
       secret)
     ))
;; makes epg don't use graphical password prompt
(setenv "GPG_AGENT_INFO" nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convert a buffer from dos ^M end of lines to unix end of lines
(defun dos2unix ()
  (interactive)
    (goto-char (point-min))
      (while (search-forward "\r" nil t) (replace-match "")))
;; vice versa
(defun unix2dos ()
  (interactive)
    (goto-char (point-min))
      (while (search-forward "\n" nil t) (replace-match "\r\n")))
;; insert current date
(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%a %b %e, %Y %l:%M %p")))
;; show ascii table
(defun ascii-table ()
  "Print the ascii table. Based on a defun by Alex Schroeder <asc@bsiag.com>"
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let ((i 0))
    (while (< i 254)
      (setq i (+ i 1))
      (insert (format "%4d %c\n" i i))))
  (beginning-of-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this command quits the minibuffer no matter where the current focus is
(defun minibuffer-quit ()
  "Quit the minibuffer command, even when the minibuffer loses focus."
  (interactive)
  (when (active-minibuffer-window)
    (save-window-excursion
      (select-window (minibuffer-window))
      (keyboard-escape-quit))))
(global-set-key (kbd "C-M-g") 'minibuffer-quit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; adds filename patterns for the major mode
;; KLUDGE: write more generic helper accepting alist
(defun add-to-auto-mode-alist (mode patterns)
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set up url-proxy-services
(require 'url)

(defun negval/parse-url (url)
  (let ((regex (rx ;; protocol
                   (group "http") "://"
                   ;; user:password@
                   (opt (+ (or alnum digit)) ":" (+ (or alnum digit)) "@")
                   ;; host:port
                   (group (+ (or alnum digit "."))) (opt ":" (group (+ digit))))))
    (string-match regex url)
    (let ((protocol (match-string-no-properties 1 url))
          (host (match-string-no-properties 2 url))
          (port (or (match-string-no-properties 3 url) "3128")))
      (values protocol host port))))

(mapc (lambda (pair)
        (let ((service (car pair))
              (url (getenv (cdr pair))))
          (if url
              (multiple-value-bind (proto host port) (negval/parse-url url)
                (setf url-proxy-services
                      (acons service (concat host ":" port) url-proxy-services))))))
      '(("http" . "http_proxy") ("ftp" . "ftp_proxy")))

;;; helpers.el ends here

;;; -*- mode: emacs-lisp; -*-
;;; Time-stamp: "2010-01-23 21:17:11 (dennis)"
;;;
;;; TODO: сделать некую систему режимов, аля (i-am-at 'home) или (i-am-at 'mfi)
;;;       если работа происходит с файлами в ~/work/mfi/projects, то грузить
;;;       соответствующий режим. По умолчанию home. (:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jump to the debugger when an error is found
(setq debug-on-error t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; where I store my elisp stuff
(setq load-path
      (nconc '("~/.emacs.d/elisp"
               "~/.emacs.d/elisp/color-theme"
               "~/.emacs.d/elisp/doxymacs/lisp"
               "~/.emacs.d/elisp/dtrt-indent"
               "~/.emacs.d/elisp/jabber"
               "~/.emacs.d/elisp/magit"
               "~/.emacs.d/elisp/nav"
               "~/.emacs.d/elisp/rails"
               "~/.emacs.d/elisp/ruby"
               )
             load-path
             ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load helpers
(load-file "~/.emacs.d/elisp.d/helpers.el")
(load-file "~/.emacs.d/elisp.d/constants.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set up url-proxy-services
(require 'url)
(require 'cl)

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load some files
(require-maybe 'generic-x)  ; nice mode for config-files
(require 'rails)
(require 'php-mode)
(require 'diff-plus-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-install (http://www.emacswiki.org/emacs/AutoInstall)
(when (require-maybe 'auto-install)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;;(auto-install-update-emacswiki-package-name nil)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recent files
;; (when (require-maybe 'recentf)
;;   (progn
;;     (recentf-mode t)
;;     (setq recentf-max-saved-items 500)
;;     (setq recentf-max-menu-items 60)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ms-windows specific settings
;; when running on windows, set the face explicitely (no regedit)
;; http://www.emacswiki.org/cgi-bin/wiki/MsWindowsRegistry
(when +win32-p+
  (progn
    (set-face-font
     'default "-outline-Consolas-normal-r-normal-normal-13-97-96-96-c-*-iso8859-5")
    ;; by default; start with 80x30 frames; FIXME: this conflicts with vm
    (add-to-list 'default-frame-alist '(height . 60))      ; 30 lines
    (add-to-list 'default-frame-alist '(width . 100))      ; 80 columns
    ;; NOTE: for X, we use ~/.Xresources for the fonts; it's faster

    (setq frame-maximized t)
    (defun w32-restore-frame ()
      "Restore a minimized frame"
      (w32-send-sys-command 61728))
    (defun w32-maximize-frame ()
      "Maximize the current frame"
      (w32-send-sys-command 61488))
    (defun w32-maximize-or-restore-frame ()
      (interactive)
      (if frame-maximized
          (w32-maximize-frame))
      (unless frame-maximized
        (w32-restore-frame))
      (setq frame-maximized (not frame-maximized)))
    (global-set-key [C-f4] 'w32-maximize-or-restore-frame)

    ;; TODO: set printer
    ;; Use this if you have a network printer
    ;; (setq printer-name "//red-prn-12/corp0066")

    ;; Use this if you have GhostScript 5.50 installed
    ;; (setenv "GS_LIB" "d:\\gstools\\gs5.50;d:\\gstools\\gs5.50\\fonts")
    ;; (setq ps-lpr-command "d:/gstools/gs5.50/gswin32c")
    ;; (setq ps-lpr-switches '("-q" "-dNOPAUSE" "-dBATCH" "-sDEVICE=mswinprn2"))
    ;; (setq ps-printer-name t)
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jump to the matching parenthesis
(defun paren-match ()
  "Tries to jump to the matching parenthesis to the one currently under the point."
  " Useful if the matching paren is out of sight."
  (interactive)
  (cond
   ((looking-at "[{\[\(]") (forward-sexp 1) (backward-char))
   ((looking-at "[]})]")   (forward-char) (backward-sexp 1))
   (t (insert "%"))))
(global-set-key (kbd "%") 'paren-match)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global keybindings
;;     the arg to 'kbd' is what you get when pushing C-h k and the key(s)
(global-set-key (kbd "<backspace>") 'delete-backward-char) ; bs => bs
(global-set-key (kbd "<delete>")    'delete-char)  ; delete == delete
(global-set-key (kbd "M-g")         'goto-line)    ; M-g  'goto-line

;; C-pgup goes to the start, C-pgdw goes to the end
(global-set-key [C-prior] '(lambda nil
                             (interactive)
                             (goto-char (point-min))))
(global-set-key [C-next] '(lambda nil
                            (interactive)
                            (goto-char (point-max))))

;; step through errors; 's' is the Hyper or 'windows' key
(global-set-key (kbd "<C-s-up>")   'previous-error)
(global-set-key (kbd "<C-s-down>") 'next-error)

;; function keys
;; (global-set-key (kbd "<f11>")  'negval/full-screen-toggle)
(global-set-key (kbd "<f12>")  'recentf-open-files)

(defmacro negval/term-program (name use-existing &optional key)
  "* macro to make a defun to start some term progr PRG, and optionally,"
  " add a keybinding to it"
  `(progn (defun ,name () (interactive)
            (negval/term-start-or-switch (format "%S" ',name) ,use-existing))
          (when ,key (global-set-key ,key ',name))))

;; will create an interactive function 'zsh', and bind it to s-<F1>
;; 's' is the "windows-key"
(negval/term-program zsh t (kbd "s-<f1>"))  ; the ubershell

(global-set-key (kbd "s-<f2>") 'gnus)
(global-set-key (kbd "s-<f5>")  (lambda () (interactive) (find-file "~/projects/")))
(global-set-key (kbd "s-<f6>")  (lambda () (interactive) (find-file "~/documents/org")))
(global-set-key (kbd "s-<f9>")  (lambda () (interactive) (switch-to-buffer "*scratch*")))
(global-set-key (kbd "s-<f10>") (lambda () (interactive) (find-file "~/.emacs")))
(global-set-key (kbd "s-<f11>") (lambda () (interactive) (find-file "~/.emacs.d/elisp/")))
(global-set-key (kbd "s-<f12>") (lambda () (interactive) (find-file "~/.emacs.d/elisp.d/")))

(global-set-key (kbd "<f5>") 'revert-buffer)

;; *fast* linenumbers on the left (unlike setnu.el)
;; http://www.emacsblog.org/2007/03/29/quick-tip-line-numbering/
(when (require-maybe 'linum)
  (global-set-key (kbd "<f6>") 'linum-mode))

(global-set-key (kbd "<f7>") 'compile)

;; ignore C-z, i keep on typing it accidentaly...
(global-set-key (kbd "C-z") nil)

;; make C-c C-c and C-c C-u work for comment/uncomment region in all modes
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)

;; zooming in and zooming out in emacs like in firefox
;; zooming; inspired by http://blog.febuiles.com/page/2/
;; (defun negval/zoom (n) (interactive)
;;   (set-face-attribute 'default (selected-frame) :height
;;     (+ (face-attribute 'default :height) (* (if (> n 0) 1 -1) 10))))

;; (global-set-key (kbd "C-+")      '(lambda()(interactive(negval/zoom 1))))
;; (global-set-key [C-kp-add]       '(lambda()(interactive(negval/zoom 1))))
;; (global-set-key (kbd "C--")      '(lambda()(interactive(negval/zoom -1))))
;; (global-set-key [C-kp-subtract]  '(lambda()(interactive(negval/zoom -1))))

;; cicle through buffers with Ctrl-Tab (like Firefox)
;; TODO: some smarter version that ignores certain buffers, see:
;; http://www.emacswiki.org/cgi-bin/wiki/ControlTABbufferCycling
(global-set-key [(control tab)] 'other-window)

;; isearch - the defaults are _so_ annoying... (well, not really global but..)
(define-key isearch-mode-map (kbd "<backspace>") 'isearch-del-char) ; bs == bs
(define-key isearch-mode-map (kbd "<delete>") 'isearch-delete-char) ; del == del

;; be able to do Ctrl-X, u/l  to upper/lowercase regions without confirm
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros to save me some type creating keyboard macros
(defmacro set-key-func (key expr)
  "macro to save me typing"
  `(local-set-key (kbd ,key)
        (lambda () (interactive) ,expr)))

(defmacro set-key (key str) `(local-set-key (kbd ,key) ,str))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general settings
(load-file "~/.emacs.d/elisp.d/general.el")
(load-file "~/.emacs.d/elisp.d/x.el")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages and settings
(load-file "~/.emacs.d/elisp.d/tramp.el")
(load-file "~/.emacs.d/elisp.d/cedet.el")
(load-file "~/.emacs.d/elisp.d/ecb.el")
(load-file "~/.emacs.d/elisp.d/emms.el")
(load-file "~/.emacs.d/elisp.d/flymake.el")
(load-file "~/.emacs.d/elisp.d/git.el")
(load-file "~/.emacs.d/elisp.d/gnus.el")
(load-file "~/.emacs.d/elisp.d/greek.el")
(load-file "~/.emacs.d/elisp.d/ispell.el")
(load-file "~/.emacs.d/elisp.d/iswitchb.el")
(load-file "~/.emacs.d/elisp.d/jabber.el")
(load-file "~/.emacs.d/elisp.d/nav.el")
(load-file "~/.emacs.d/elisp.d/org.el")
(load-file "~/.emacs.d/elisp.d/saveplace.el")
(load-file "~/.emacs.d/elisp.d/template-file.el")
(load-file "~/.emacs.d/elisp.d/twitter.el")
(load-file "~/.emacs.d/elisp.d/w3m.el")
(load-file "~/.emacs.d/elisp.d/undo-tree.el")
(load-file "~/.emacs.d/elisp.d/yasnippet.el")
;; TODO: sr-select-window: Wrong type argument: window-live-p, nil
;; (load-file "~/.emacs.d/elisp.d/sunrise-commander.el")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; time/date/calendar stuff
(setq holidays-in-diary-buffer          t
      mark-holidays-in-calendar         t
      all-christian-calendar-holidays   t)
(setq display-time-24hr-format t
      display-time-day-and-date nil
      display-time-format nil
      default-indicate-empty-lines t
      display-time-use-mail-icon t
      display-time-load-average-threshold 20)
(display-time)

(setq calendar-latitude 56.19)
(setq calendar-longitude 44.00)
(setq calendar-location-name "Nizhniy Novgorod, Russia")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modes
(load-file "~/.emacs.d/elisp.d/prog.el")

(load-file "~/.emacs.d/elisp.d/c-cxx.el")
(load-file "~/.emacs.d/elisp.d/cmake.el")
(load-file "~/.emacs.d/elisp.d/clojure.el")
(load-file "~/.emacs.d/elisp.d/slime.el")
(load-file "~/.emacs.d/elisp.d/emacs-lisp.el")
(load-file "~/.emacs.d/elisp.d/erlang.el")
(load-file "~/.emacs.d/elisp.d/forth.el")
(load-file "~/.emacs.d/elisp.d/haskell.el")
(load-file "~/.emacs.d/elisp.d/lisp.el")
(load-file "~/.emacs.d/elisp.d/makefile.el")
(load-file "~/.emacs.d/elisp.d/perl.el")
(load-file "~/.emacs.d/elisp.d/rpm.el")
(load-file "~/.emacs.d/elisp.d/ruby.el")
(load-file "~/.emacs.d/elisp.d/scheme.el")
(load-file "~/.emacs.d/elisp.d/sh.el")
(load-file "~/.emacs.d/elisp.d/tex.el")
(load-file "~/.emacs.d/elisp.d/text.el")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compilation; if compilation is successful, autoclose the compilation win
;; http://www.emacswiki.org/cgi-bin/wiki/ModeCompile
;; TODO: don't hide when there are warnings either (not just errors)
(setq compilation-window-height 12)
;; (setq compilation-finish-functions 'compile-autoclose)
;; (defun compile-autoclose (buffer string)
;;   (cond ((and (string-match "finished" string)
;;            (not (string-match "warning" string)))
;;           (message "Build maybe successful: closing window.")
;;           (run-with-timer 2 nil
;;             'delete-window
;;             (get-buffer-window buffer t)))
;;     (t
;;       (message "Compilation exited abnormally: %s" string))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customization for term, ansi-term
(defun negval/term-mode-hook ()
  (interactive)
  ;; turn it off, just for this buffer -- thanks to snogglethorpe in #emacs
  (set (make-local-variable 'global-hl-line-mode) nil)
  (local-set-key [(tab)] nil))
(add-hook 'term-mode-hook 'negval/term-mode-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start as server; thus, we can use emacs for mutt, without
;; starting a new instance for each mail, see:
;; http://www.emacswiki.org/cgi-bin/emacs-en/MuttInEmacs
(server-start)

;; don't want to use C-x # when closing the client, just C-x k as always
(add-hook 'server-switch-hook
          (lambda ()
            (local-set-key (kbd "C-x k") 'server-edit)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; safe locals; we mark these as 'safe', so emacs22+ won't give us annoying
;; warnings
(setq safe-local-variable-values
      (quote ((auto-recompile . t)
              (outline-minor-mode . t)
              auto-recompile outline-minor-mode)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elisp function/macros
;; switch to a buffer it already exists, otherwise return nil
(defun negval/switch-to-named-buffer (name)
  "* try to select buffer with NAME from the buffer list; evaluate to t"
  "  if buffer was found, nil otherwise"
  (interactive)
  (defun negval/switch-buffer (lst name)
    (if lst
        (let ((curbuf (buffer-name (car lst))))
          (if (string= curbuf name)
              (progn (switch-to-buffer curbuf) t)
            (negval/switch-buffer (cdr lst) name)))
      nil))
  (negval/switch-buffer (buffer-list) name))

(defun negval/term-start-or-switch (prg &optional use-existing)
  "* run program PRG in a terminal buffer. If USE-EXISTING is non-nil "
  " and PRG is already running, switch to that buffer instead of starting"
  " a new instance. Optional give a keybinding in KEY"
  (interactive)
  (when (not (and use-existing
                (negval/switch-to-named-buffer (format "*%s*" prg))))
    (ansi-term prg prg)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file "~/.emacs.d/elisp.d/sudosave.el")

(custom-set-variables
 '(mouse-yank-at-point t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

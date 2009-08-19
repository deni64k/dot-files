;;; -*- mode: emacs-lisp; -*-
;;; Time-stamp: "2009-08-16 22:57:20 (dennis)"
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
      (nconc load-path
             '("~/.emacs.d/elisp"
               "~/.emacs.d/elisp/color-theme"
               "~/.emacs.d/elisp/dtrt-indent"
               "~/.emacs.d/elisp/jabber"
               "~/.emacs.d/elisp/magit"
               "~/.emacs.d/elisp/nav"
               "~/.emacs.d/elisp/rails"
               "~/.emacs.d/elisp/ruby"
               )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load helpers
(load-file "~/.emacs.d/elisp.d/helpers.el")
(load-file "~/.emacs.d/elisp.d/constants.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the file-coding-system-alist
(add-to-list 'file-coding-system-alist (cons "/core400/" 'koi8-r))
(add-to-list 'file-coding-system-alist (cons "/db_agent/" 'koi8-r))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the modeline
(line-number-mode t)                      ; show line numbers
(column-number-mode t)                    ; show column numbers
(when-available 'size-indication-mode
  (size-indication-mode t))               ; show file size (emacs 22+)
(display-time-mode t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general settings

;; this sets garbage collection to hundred times of the default
;; supposedly significantly speeds up startup time
;; (setq gc-cons-threshold 10000)

;; turns off blink cursor
(when-available 'blink-cursor-mode
  (blink-cursor-mode 0))

(transient-mark-mode t)         ; make the current 'selection' visible
(delete-selection-mode t)       ; delete the selection area with a keypress
(setq search-highlight t        ; highlight when searching...
      query-replace-highlight t)    ; ...and replacing
(fset 'yes-or-no-p 'y-or-n-p)   ; enable one letter y/n answers to yes/no

(defalias 'qrr 'query-replace-regexp) ; search and replace by regular expresion

(global-font-lock-mode t)         ; always do syntax highlighting
(when (require-maybe 'jit-lock)   ; enable JIT to make font-lock faster
  (setq jit-lock-stealth-time 1)) ; new with emacs21

(set-language-environment "UTF-8") ; prefer utf-8 for language settings

;; the autosave is typically done by keystrokes, but I'd like to save
;; after a certain amount of time as well
(setq auto-save-timeout 1800)

(setq scroll-conservatively 10000)  ; smooth scrolling

(setq completion-ignore-case t      ; ignore case when completing...
      read-file-name-completion-ignore-case t) ; ...filenames too
(icomplete-mode t)

(put 'narrow-to-region 'disabled nil) ; enable...
(put 'erase-buffer 'disabled nil)     ; ... useful things

(when-available 'file-name-shadow-mode ; emacs22+
  (file-name-shadow-mode 1))           ; be smart about filenames
                                        ; (understand ~/ etc.)
(when-available 'set-fringe-mode    ; emacs22+
  (progn
    (set-fringe-mode '(nil . 0))
    (setq indicate-buffer-boundaries 'left)
    (custom-set-faces
     '(fringe
       ((((class color) (background dark)) (:background "grey10"))
        (((class color) (background light)) (:background "snow")))))
    ))

(require-maybe 'generic-x)         ; nice mode for config-files

(mouse-wheel-mode t)            ; turn on mouse's wheel

;; focus over mouse
;; (when (require-maybe 'follow-mouse)
;;   (turn-on-follow-mouse))

;; "don't hscroll unless needed"- ? More voodoo lisp.
(setq hscroll-margin 1)

;; "Redefine the Home/End keys to (nearly) the same as visual studio
;; behavior... special home and end by Shan-leung Maverick WOO
;; <sw77@cornell.edu>"
;; This is complex. In short, the first invocation of Home/End moves
;; to the beginning of the *text* line. A second invocation moves the
;; cursor to the beginning of the *absolute* line. Most of the time
;; this won't matter or even be noticeable, but when it does (in
;; comments, for example) it will be quite convenient.
(global-set-key [home] 'negval/smart-home)
(global-set-key [end] 'negval/smart-end)
(defun negval/smart-home ()
  "Odd home to beginning of line, even home to beginning of
text/code."
  (interactive)
  (if (and (eq last-command 'negval/smart-home)
           (/= (line-beginning-position) (point)))
      (beginning-of-line)
    (beginning-of-line-text))
  )
(defun negval/smart-end ()
  "Odd end to end of line, even end to begin of text/code."
  (interactive)
  (if (and (eq last-command 'negval/smart-end)
           (= (line-end-position) (point)))
      (end-of-line-text)
    (end-of-line))
  )
(defun end-of-line-text ()
  "Move to end of current line and skip comments and trailing space.
Require `font-lock'."
  (interactive)
  (end-of-line)
  (let ((bol (line-beginning-position)))
    (unless (eq font-lock-comment-face (get-text-property bol 'face))
      (while (and (/= bol (point))
                  (eq font-lock-comment-face
                      (get-text-property (point) 'face)))
        (backward-char 1))
      (unless (= (point) bol)
        (forward-char 1) (skip-chars-backward " \t\n"))))
  ) ;; Done with home and end keys.
;; But what about the normal use for home and end?
;; We can still have them! Just prefixed with control.
(global-set-key [C-home] 'beginning-of-line)
(global-set-key [C-end] 'end-of-line)

;; What it says. Keeps the cursor in the same relative row during
;; pgups and dwns.
(setq scroll-preserve-screen-position t)

;; don't show startup messages
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; define dirs for cacheing file dirs
;; see http://www.emacswiki.org/cgi-bin/wiki/FileNameCache
;; for more tricks with this...
(when-available 'file-cache-add-directory   ; emacs 22+
  (progn
    (defvar cachedirs
      '("~/" "/etc/"))
    (dolist (dir cachedirs) (file-cache-add-directory dir))))

;; set frame title / icon title using filename or buffername
;; little trick (based on http://www.emacswiki.org/cgi-bin/wiki/ShadyTrees)
;; to replace  /home/foo with ~
(defun negval/title-format ()
  (if buffer-file-name
      (replace-regexp-in-string "\\\\" "/"
                                (replace-regexp-in-string (regexp-quote (getenv "HOME")) "~"
                                                          (convert-standard-filename buffer-file-name)))
    (buffer-name)))
(setq
 frame-title-format '(:eval (negval/title-format))
 icon-title-format  '(:eval (concat "emacs:" (negval/title-format))))

;; files without terminating newlines really annoy me
(setq require-final-newline t)

;; editing .xpi files directly in Emacs
(setq auto-mode-alist (cons '("\\.xpi$" . archive-mode) auto-mode-alist))

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
;; change cursor color based on mode
;; http://www.emacswiki.org/cgi-bin/wiki/download/cursor-chg.el
(setq hcz-set-cursor-color-color "")
(setq hcz-set-cursor-color-buffer "")
(defun hcz-set-cursor-color-according-to-mode ()
  "change cursor color according to some minor modes."
  ;; set-cursor-color is somewhat costly, so we only call it when
  ;; needed:
  (let ((color
         (if buffer-read-only "white"
           (if overwrite-mode "red" "SteelBlue"))))
    (unless (and
             (string= color hcz-set-cursor-color-color)
             (string= (buffer-name) hcz-set-cursor-color-buffer))
      (set-cursor-color (setq hcz-set-cursor-color-color color))
      (setq hcz-set-cursor-color-buffer (buffer-name)))))
(add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode)
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
;; (negval/term-program mutt   t (kbd "s-<f2>"))  ; console mail client
;; (negval/term-program irssi  t (kbd "s-<f3>"))  ; console irc client
;; (negval/term-program slrn   t (kbd "s-<f4>"))  ; console nttp client
;; (negval/term-program raggle t (kbd "s-<f5>"))  ; console feedreader

;; make s-<f10> switch to *scratch*
(global-set-key (kbd "s-<f10>") (lambda nil (interactive) (switch-to-buffer "*scratch*")))
(global-set-key (kbd "s-<f11>") (lambda nil (interactive) (find-file "~/.emacs")))
(global-set-key (kbd "s-<f12>") (lambda nil (interactive) (find-file "~/.emacs.d/elisp/")))

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
;; iswitchb
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SavePlace: this puts the cursor in the last place you editted
;; a particular file. This is very useful for large files.
(when (require-maybe 'saveplace)
  (progn
    (setq-default save-place t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tabbar (no use)
;; (when (require-maybe 'tabbar)
;;   (progn
;;     (tabbar-mode t)
;;     (global-set-key [C-M-tab] 'tabbar-forward)
;;     (global-set-key [C-M-S-iso-lefttab] 'tabbar-backward)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniquify
(when (require-maybe 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward
        uniquify-strip-common-suffix 't))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  abbrevs (emacs will automagically expand abbreviations)
;;
(abbrev-mode t)                 ; enable abbrevs (abbreviations) ...
(add-hook 'kill-emacs-hook      ; ... end save them upon emacs exit
          (lambda () (write-abbrev-file abbrev-file-name)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; backups  (emacs will write backups and number them)
(setq make-backup-files t ; do make backups
      backup-by-copying t ; and copy them ...
      backup-directory-alist '(("." . "~/.emacs.d/backups")) ; ... here
      version-control t
      kept-new-versions 2
      kept-old-versions 5
      delete-old-versions t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; time-stamps
;; when there is a "Time-stamp: <>" in the first 10 lines of the file,
;; emacs will write time-stamp information there when saving the file.
;; see the top of this file for an example...
(setq
 time-stamp-active t          ; do enable time-stamps
 time-stamp-line-limit 10     ; check first 10 buffer lines for Time-stamp: <>
 time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)") ; date format
(add-hook 'write-file-hooks 'time-stamp) ; update when saving
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros to save me some type creating keyboard macros
(defmacro set-key-func (key expr)
  "macro to save me typing"
  (list 'local-set-key (list 'kbd key)
        (list 'lambda nil
              (list 'interactive nil) expr)))

(defmacro set-key (key str) (list 'local-set-key (list 'kbd key) str))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nav is a lightweight solution for Emacs users who want something like
;; TextMate's file browser, or the Eclipse project view. Unlike these two,
;; Nav only shows the contents of a single directory at a time, but it allows
;; recursive searching for filenames using the 'f' key-binding, and recursive
;; grepping of file contents with the 'g' key-binding.
(require-maybe 'nav)

;; sunrise-commander: two-pane file manager
;; TODO: sr-select-window: Wrong type argument: window-live-p, nil
;; (when (require-maybe 'sunrise-commander)
;;   (sunrise-mc-keys))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general settings
(load-file "~/.emacs.d/elisp.d/x.el")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages and settings
(load-file "~/.emacs.d/elisp.d/cedet.el")
(load-file "~/.emacs.d/elisp.d/ecb.el")
(load-file "~/.emacs.d/elisp.d/emms.el")
(load-file "~/.emacs.d/elisp.d/flymake.el")
(load-file "~/.emacs.d/elisp.d/git.el")
(load-file "~/.emacs.d/elisp.d/greek.el")
(load-file "~/.emacs.d/elisp.d/ispell.el")
(load-file "~/.emacs.d/elisp.d/jabber.el")
(load-file "~/.emacs.d/elisp.d/org.el")
(load-file "~/.emacs.d/elisp.d/template-file.el")
;;(load-file "~/.emacs.d/elisp.d/tramp.el")
(load-file "~/.emacs.d/elisp.d/twitter.el")
(load-file "~/.emacs.d/elisp.d/w3m.el")
(load-file "~/.emacs.d/elisp.d/yasnippet.el")
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
(load-file "~/.emacs.d/elisp.d/emacs-lisp.el")
(load-file "~/.emacs.d/elisp.d/erlang.el")
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; if we try to save a file owned by someone else, use sudo
;; http://www.emacswiki.org/cgi-bin/wiki/SudoSave
(when (require-maybe 'sudo)
  (defun sudo-before-save-hook ()
    (set (make-local-variable 'sudo:file) (buffer-file-name))
    (when sudo:file
      (unless (file-writable-p sudo:file)
        (set (make-local-variable 'sudo:old-owner-uid)
          (nth 2 (file-attributes sudo:file)))
        (when (numberp sudo:old-owner-uid)
          (unless (= (user-uid) sudo:old-owner-uid)
            (when (y-or-n-p
                    (format "File %s is owned by %s, save it with sudo? "
                      (file-name-nondirectory sudo:file)
                      (user-login-name sudo:old-owner-uid)))
              (sudo-chown-file (int-to-string (user-uid))
                (sudo-quoting sudo:file))
              (add-hook 'after-save-hook
                (lambda ()
                  (sudo-chown-file (int-to-string sudo:old-owner-uid)
                    (sudo-quoting sudo:file))
                  (if sudo-clear-password-always
                    (sudo-kill-password-timeout)))
                nil   ;; not append
                t           ;; buffer local hook
                )))))))
  (add-hook 'before-save-hook 'sudo-before-save-hook))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "I always compile my .emacs, saves me about two seconds
;; startuptime. But that only helps if the .emacs.elc is newer
;; than the .emacs. So compile .emacs if it's not."
(defun negval/auto-recompile-file-always ()
  (when (equal mode-name "Emacs-Lisp")
    (let ((maximum-compile-log-height 8)
          (old-window-start (window-start))
          (old-window-point (window-point)))
      ;; pre-split for compile log buffer so that it does not modify the layout
      (set-window-buffer (split-window-vertically (- (window-height) maximum-compile-log-height)) (get-buffer-create "*Compile-Log*"))
      ;; byte compile the buffer
      (byte-compile-file buffer-file-name)
      (let ((buf (get-buffer "*Compile-Log*")))
        ;; scroll to the end to see if there's an error
        (set-window-point (get-buffer-window buf) (buffer-size buf))
        ;; auto close the compile log window and restore original display position
        (run-at-time 1.0 nil (lambda (buf)
                               (delete-windows-on buf)
                               (set-window-point (selected-window) old-window-point)
                               (set-window-start (selected-window) old-window-start))
                     buf)))))

(defun add-after-save-hook ()
  (make-local-hook 'after-save-hook)
  (add-hook 'after-save-hook 'negval/auto-recompile-file-always))

(add-hook 'emacs-lisp-mode-hook 'add-after-save-hook)

(custom-set-variables
 '(mouse-yank-at-point t))

(setq debug-on-error nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ECB
;; Последние три строки создают привязки активизации / деактивизации модуля
;; и переключения редактирования кода в полноэкранный режим. M M e aвключает ECB,
;; M M e d выгружает ECB, a M M l переключает режим окна редактирования кода.
;(require 'ecb)
;(global-set-key (kbd "\e\el") 'ecb-toggle-ecb-windows)
;(global-set-key (kbd "\e\eea") 'ecb-activate)
;(global-set-key (kbd "\e\eed") 'ecb-deactivate)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-layout-name "left1")
 '(ecb-options-version "2.32")
 '(ecb-source-path (quote ("~/projects/")))
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-width 0.25)
 )

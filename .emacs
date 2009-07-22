;;; -*- mode: emacs-lisp; -*-
;;; Time-stamp: "2009-07-22 13:10:44 (dsuhonin)"
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
               "~/.emacs.d/elisp/org/contrib/lisp"
               "~/.emacs.d/elisp/org/lisp"
               "~/.emacs.d/elisp/rails"
               "~/.emacs.d/elisp/ruby"
               "~/.emacs.d/elisp/w3m"
               )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load helpers
(load-file "~/.emacs.d/elisp.d/helpers.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; what kind of system are we using?  start with these, as it will influence
;; other stuff inspired by: http://www.xsteve.at/prg/emacs/.emacs.txt
(defconst +win32-p+
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")
(defconst +unix-p+
  (or (eq system-type 'berkeley-unix)
      (eq system-type 'gnu/linux)
      (eq system-type 'linux))
  "Are we running on a Unix or Unix-like system?")
(defconst +home-p+ (string-match (getenv "PLACEMENT") "home")
  "Am I at home?")
(defconst +work-p+ (string-match (getenv "PLACEMENT") "work")
  "Am I at work?")
(defun console-p nil
  "Are we running in a console (non-X) environment?"
  (eq (symbol-value 'window-system) nil))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; make epg don't use graphical password prompt
(setenv "GPG_AGENT_INFO" nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; X settings
(defun negval/x-clipboard nil
  (setq x-select-enable-clipboard t)        ; copy-paste should work ...
  (setq interprogram-paste-function         ; ...with...
        'x-cut-buffer-or-selection-value))  ; ...other X clients

(defun negval/x-colors nil
  "Color settings."
  (require 'color-theme)
  (color-theme-initialize)
  (color-theme-comidia)

  ;; highlight the current line; set a custom face, so we can
  ;; recognize from the normal marking (selection)
  ;; don't turn in on globally, only in specific modes (see negval/c-mode-hook)
  (when-available 'global-hl-line-mode
    (progn
      (custom-set-faces
       '(hl-line ((((class color) (background dark)) (:background "#111111"))
                  (((class color) (background light)) (:background "snow3")))))
      (setq hl-line-face 'hl-line)
      (global-hl-line-mode t))) ;; turn it on for all modes by default

  ;; show-paren-mode
  ;; show a subtle blinking of the matching paren (the defaults are ugly)
  ;; http://www.emacswiki.org/cgi-bin/wiki/ShowParenMode
  (when-available 'show-paren-mode
    (progn
      (custom-set-faces
       '(show-paren-match ((((class color) (background dark)) (:background "#222222"))
                           (((class color) (background light)) (:background "snow1")))))
      (setq show-paren-style 'expression)
      (show-paren-mode t)))

  (custom-set-faces
   '(widget-field ((((class color) (background dark))
                    (:background "dim gray" :foreground "light cyan"))))
   '(highlight ((((class color) (background dark))
                 (:background "dark olive green" :foreground "light cyan")))))

  (custom-set-faces
   '(header-line ((((class color) (background dark))
                   (:background "gray10" :family "Droid Sans Mono"
                    :foreground "gray90" :height 0.85 :width condensed))))
   '(mode-line ((((class color) (background dark))
                 (:background "gray10" :family "Droid Sans"
                  :foreground "gray90" :height 0.85
                  :inverse-video nil :box nil))))
   '(mode-line-inactive ((((class color) (background dark))
                          (:background "gray7" :family "Droid Sans"
                           :foreground "gray90" :height 0.85
                           :inverse-video nil :box nil))))
   '(mode-line-buffer-id ((((class color) (background dark))
                           (:background "gray15" :family "Droid Sans"
                            :width condensed
                            :foreground "green3" :box nil))))
   '(which-func ((((class color) (background dark))
                  (:background "gray15" :family "Droid Sans"
                   :width condensed
                   :foreground "cyan1")))))
  )

(defun negval/x-bars nil
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0))

;; (negval/x-clipboard)
(negval/x-colors)
(negval/x-bars)
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
           (if overwrite-mode "red" "orange"))))
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
  (global-set-key (kbd "<f6>")     'linum))

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
(load-file "~/.emacs.d/elisp.d/jabber.el")
(load-file "~/.emacs.d/elisp.d/twitter.el")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tramp, for remote access
(when +unix-p+
  (setq tramp-default-method "ssh"))
(when +win32-p+
  (setq tramp-default-method "plink"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; browse-url
(when-available 'browse-url
  (require-maybe 'w3m-load)
  (setq browse-url-browser-function 'w3m-browse-url
        w3m-use-cookies t)
  (global-set-key (kbd "C-x m") 'browse-url-at-point))
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
;; Ispell
(when (featurep 'flyspell)
  (set-default 'ispell-skip-html t)

  (setq ispell-dictionary "english")

  (defun turn-on-flyspell ()
    "Force flyspell-mode on using a positive arg. For use in hooks."
    (interactive)
    (flyspell-mode 1))

  (autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
  (add-hook 'message-mode-hook 'turn-on-flyspell)
  (add-hook 'text-mode-hook 'turn-on-flyspell)
  (add-hook 'nxml-mode-hook 'turn-on-flyspell)
  (add-hook 'texinfo-mode-hook 'turn-on-flyspell)
  (add-hook 'TeX-mode-hook 'turn-on-flyspell)

  (add-hook 'c-mode-common-hook 'flyspell-prog-mode)
  (add-hook 'lisp-mode-hook 'flyspell-prog-mode)
  (add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file "~/.emacs.d/elisp.d/prog.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yaSnippet - yet another snippet extension for Emacs
(when (require-maybe 'yasnippet)
  (yas/initialize)
  (yas/load-directory "~/.emacs.d/snippets")
  (add-hook 'yas/after-exit-snippet-hook
            '(lambda ()
               (indent-region yas/snippet-beg yas/snippet-end))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pretty greek symbols

(defun unicode-symbol (name)
  "Translate a symbolic name for a Unicode character -- e.g., LEFT-ARROW
  or GREATER-THAN into an actual Unicode character code. "
  (decode-char 'ucs (case name
                      ;; arrows
                      ('left-arrow 8592)
                      ('up-arrow 8593)
                      ('right-arrow 8594)
                      ('down-arrow 8595)

                      ;; boxes
                      ('double-vertical-bar #X2551)

                      ;; relational operators
                      ('equal #X003d)
                      ('not-equal #X2260)
                      ('identical #X2261)
                      ('not-identical #X2262)
                      ('less-than #X003c)
                      ('greater-than #X003e)
                      ('less-than-or-equal-to #X2264)
                      ('greater-than-or-equal-to #X2265)

                      ;; logical operators
                      ('logical-and #X2227)
                      ('logical-or #X2228)
                      ('logical-neg #X00AC)

                      ;; misc
                      ('nil #X2205)
                      ('horizontal-ellipsis #X2026)
                      ('double-exclamation #X203C)
                      ('prime #X2032)
                      ('double-prime #X2033)
                      ('down-right-diagonal-ellipsis #x22f1)
                      ('for-all #X2200)
                      ('there-exists #X2203)
                      ('element-of #X2208)
                      ('contains-as-member #x220b)

                      ;; mathematical operators
                      ('square-root #X221A)
                      ('squared #X00B2)
                      ('cubed #X00B3)

                      ;; letters
                      ('lambda #X03BB)
                      ('alpha #X03B1)
                      ('beta #X03B2)
                      ('gamma #X03B3)
                      ('delta #X03B4))))

(defun substitute-pattern-with-unicode (pattern symbol)
  "Add a font lock hook to replace the matched part of PATTERN with the
  Unicode symbol SYMBOL looked up with UNICODE-SYMBOL."
  (interactive)
  (font-lock-add-keywords
   nil `((,pattern (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                           ,(unicode-symbol symbol))
                           nil))))))

(defun substitute-patterns-with-unicode (patterns)
  "Call SUBSTITUTE-PATTERN-WITH-UNICODE repeatedly."
  (mapcar #'(lambda (x)
              (substitute-pattern-with-unicode (car x)
                                               (cdr x)))
          patterns))

(defun greek-symbols-in-lisp ()
  (substitute-patterns-with-unicode
   (list (cons "\\(nil\\)" 'nil)
         (cons "\\<\\(sqrt\\)\\>" 'square-root)
         (cons "\\<\\(not\\)\\>" 'logical-neg)
         (cons "\\<\\(lambda\\)\\>" 'lambda)
         (cons "\\<\\(alpha\\)\\>" 'alpha)
         (cons "\\<\\(beta\\)\\>" 'beta)
         (cons "\\<\\(gamma\\)\\>" 'gamma)
         (cons "\\<\\(delta\\)\\>" 'delta)
         (cons "\\<\\\"\\>" 'double-prime)
         )))
(add-hook 'lisp-mode-hook 'greek-symbols-in-lisp)
(add-hook 'emacs-lisp-mode-hook 'greek-symbols-in-lisp)

(add-hook 'c++-mode-hook
          (lambda ()
            (substitute-patterns-with-unicode
             (list (cons "\\(->\\)" 'right-arrow)
                   (cons "\\(==\\)" 'identical)
                   (cons "\\(\\!=\\)" 'not-equal)
                   (cons "\\<\\(sqrt\\)\\>" 'square-root)
                   (cons "\\(&&\\)" 'logical-and)
                   (cons "\\(||\\)" 'logical-or)
                   (cons "\\<\\(!\\)\\[^=\\]\\>" 'logical-neg)
                   (cons "\\(>\\)\\[^=\\]" 'greater-than)
                   (cons "\\(<\\)\\[^=\\]" 'less-than)
                   (cons "\\(>=\\)" 'greater-than-or-equal-to)
                   (cons "\\(<=\\)" 'less-than-or-equal-to)
                   (cons "\\<\\(lambda\\)\\>" 'lambda)
                   (cons "\\<\\(alpha\\)\\>" 'alpha)
                   (cons "\\<\\(beta\\)\\>" 'beta)
                   (cons "\\<\\(gamma\\)\\>" 'gamma)
                   (cons "\\<\\(delta\\)\\>" 'delta)
                   (cons "\\(\\.\\.\\.\\)" 'horizontal-ellipsis)
                   (cons "\\(\\\\\\\"\\)" 'double-prime)
                   (cons "\\(\\\\\\\\\\\)" 'down-right-diagonal-ellipsis)
                   ))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; text-mode
(defun negval/text-mode-hook ()
  (interactive)
  (set-fill-column 78)                    ; lines are 78 chars long ...
  (auto-fill-mode t)                      ; ... and wrapped around automagically

  ;; http://taiyaki.org/elisp/word-count/src/word-count.el
  (when (require-maybe 'word-count) ; count the words
    (word-count-mode t))

  (when (require-maybe 'filladapt) ; do the intelligent wrapping of lines,...
    (filladapt-mode t))) ; ... (bullets, numbering) if
                                        ; available
(add-hook 'text-mode-hook 'negval/text-mode-hook)

;; turn on autofill for all text-related modes
(toggle-text-mode-auto-fill)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rpm-spec-mode
(autoload 'rpm-spec-mode "rpm-spec-mode.el" "RPM spec mode." t)
(setq auto-mode-alist (append '(("\\.spec" . rpm-spec-mode))
                              auto-mode-alist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeX/LaTex
(defun negval/tex-mode-hook ()
  (interactive)

  (setq TeX-parse-self t) ; Enable parse on load.
  (setq TeX-auto-save t) ; Enable parse on save.

  (set-key-func "C-c 1"  (negval/tex-tag-region-or-point-outside "section"))
  (set-key-func "C-c 2"  (negval/tex-tag-region-or-point-outside "subsection"))
  (set-key-func "C-c 3"  (negval/tex-tag-region-or-point-outside "subsubsection"))

  (set-key-func "C-c C-a l"  (negval/tex-tag-region-or-point-outside "href{}"))

  (set-key-func "C-c i"  (negval/tex-tag-region-or-point "em"))
  (set-key-func "C-c b"  (negval/tex-tag-region-or-point "bf"))
  (set-key-func "C-c s"  (negval/tex-tag-region-or-point "small"))
  (set-key-func "C-c u"  (negval/tex-tag-region-or-point "underline"))
  (set-key-func "C-c tt" (negval/tex-tag-region-or-point "tt")))

(add-hook 'tex-mode-hook 'negval/tex-mode-hook)
(add-hook 'LaTeX-mode-hook 'negval/tex-mode-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; some TeX/LaTeX-related functions
(defun negval/tex-tag-region (b e tag)
  "put '{\tag...}' around text"
  (let ((tb (concat "{\\" tag " ")))
    (insert
     (concat tb (delete-and-extract-region b e) "}"))
    (goto-char (- (point) 1))))

(defun negval/tex-tag-region-or-point (el)
  "tag the region or the point if there is no region"
  (when (not mark-active)
    (set-mark (point)))
  (negval/tex-tag-region (region-beginning) (region-end) el))

(defun negval/tex-tag-region-outside (b e tag)
  "put '{\tag...}' around text"
  (let ((tb (concat "\\" tag "{")))
    (insert
     (concat tb (delete-and-extract-region b e) "}"))
    (goto-char (- (point) 1))))

(defun negval/tex-tag-region-or-point-outside (el)
  "tag the region or the point if there is no region"
  (when (not mark-active)
    (set-mark (point)))
  (negval/tex-tag-region-outside (region-beginning) (region-end) el))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Lisp
(defun negval/emacs-lisp-mode-hook ()
  (interactive)
  ;; overrides the global f7 for compilation
  (local-set-key (kbd "<f7>") 'eval-buffer)
  (local-set-key (kbd "<C-f7>") 'eval-region)

  (setq lisp-indent-offset '+
        indent-tabs-mode t)
  (abbrev-mode 1)
  (set-fill-column 100)
  (auto-fill-mode 1)
  (local-set-key (kbd "RET") 'newline-and-indent))

;; show some functions as keywords
(font-lock-add-keywords 'emacs-lisp-mode
                        '(("\\<\\(quote\\|add-hook\\)" .
                           font-lock-keyword-face)))
;; recognize some things as functions
(font-lock-add-keywords 'emacs-lisp-mode
                        '(("\\<\\(set\\|setq\\|require-maybe\\|when-available\\|add-hook\\)\\>" .
                           font-lock-function-name-face)))
;; recognize some things as constants
(font-lock-add-keywords 'emacs-lisp-mode
                        '(("\\<\\(nil\\|\\t\\)\\_>" .
                           font-lock-constant-face)))

(font-lock-add-keywords 'emacs-lisp-mode
                        '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):"
                           1 font-lock-warning-face prepend)))

(add-hook 'emacs-lisp-mode-hook 'negval/emacs-lisp-mode-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file "~/.emacs.d/elisp.d/lisp.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme
(setq scheme-program-name "snow")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perl/cperl mode
;; TODO: get rid of the annoying auto )]}
(defalias 'perl-mode 'cperl-mode) ; cperl mode is what we want

(defun negval/cperl-mode-hook ()
  (interactive)
  (eval-when-compile (require 'cperl-mode))
  (setq
   cperl-hairy t                  ; parse hairy perl constructs
   cperl-indent-level 2           ; indent with 2 positions
   cperl-invalid-face (quote off) ; don't show stupid underlines
   cperl-electric-keywords t))    ; complete keywords

(add-hook 'cperl-mode-hook 'negval/cperl-mode-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c-mode / c++-mode
(defconst negval/c-style
  '((c-tab-always-indent . t)
    (c-basic-offset . 2)
    (ident-tabs-mode . t)
    (c-offsets-alist
     (brace-list-open . +)
     (brace-list-intro . +)
     (brace-list-entry . -)
     (case-label . +)
     (inline-open . 0)
     (innamespace . 0)
     (statement-cont . +)
     (substatement-open . 0)
     (topmost-intro-cont . 0)
     )))
(defconst mfisoft:c-style
  '((c-tab-always-indent . t)
    (c-basic-offset . 4)
    (ident-tabs-mode . nil)
    (c-offsets-alist
     (brace-list-open . +)
     (case-label . +)
     (inline-open . 0)
     (innamespace . 0)
     (statement-cont . +)
     (substatement-open . 0)
     (topmost-intro-cont . 0)
     )))

(defun include-separator ()
  "include separator like //--...-"
  (interactive)
  (insert "//")
  (dotimes (i 78)
    (insert "-"))
  (insert "\n"))

(defun include-guards ()
  "include the #ifndef/#define/#endif include guards for the current buffer"
  (interactive)
  (let ((tag (concat "__"
                     (mapconcat (lambda (s) (upcase s))
                                (split-string
                                 (if (string-match "/src/" buffer-file-name)
                                     (car (reverse (split-string buffer-file-name "/src/")))
                                   '(buffer-name))
                                 "/\\|_\\|-\\|\\.") "_")  "__")))
    (insert (concat "#ifndef " tag "\n"))
    (insert (concat "#define " tag "\n\n"))
    (insert (concat "#endif /* " tag " */\n"))))

(defun mfisoft:include-guards ()
  "include the #ifndef/#define/#endif include guards for the current buffer"
  (interactive)
  (let ((tag (mapconcat (lambda (s) (upcase s))
                        (split-string
                         (if (string-match "/src/" buffer-file-name)
                             (car (reverse (split-string buffer-file-name "/src/")))
                           '(buffer-name))
                         "/\\|_\\|-\\|\\.") "_")))
    (insert (concat "#ifndef " tag "\n"))
    (insert (concat "#define " tag "\n\n"))
    (insert (concat "#endif // " tag "\n"))))

(defun include-timestamp ()
  "include timestamp"
  (interactive)
  (insert "/* Time-stamp: <> */\n"))

(defun include-gplv3 ()
  "include GPLv2 license header"
  (interactive)
  (insert
   "/*
 * Copyright (C) 2009 Denis Sukhonin <openlunatic@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */"))

(defun include-lgplv3 ()
  "include LGPLv3 license header"
  (interactive)
  (insert
   "/*
 * Copyright (C) 2009 Denis Sukhonin <openlunatic@gmail.com>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3
 * of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301, USA.
 *
 */"))

(defun include-bsd ()
  "include modern BSD license header"
  (interactive)
  (insert
   "/*
 * Copyright (c) 2009 Denis Sukhonin <openlunatic@gmail.com>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */\n"))

(defun mfisoft:include-header ()
  "include MFI Soft's header"
  (interactive)
  (insert
   "/**
    \file
    \brief
 */\n\n"))

(defun include-header ()
  "include my header"
  (interactive)
  (insert
   "/* -*- coding: utf-8; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 2 -*- */
/*
 * Written by Denis Sukhonin <openlunatic@gmail.com>
 * 100% Public Domain - no warranty.
 *
 * $Id$
 */\n"))

;; other customizations
(defun negval/update-tagfile ()
  "try to find the top-directory of the current path, and create/update "
  "the tagfile "
  (interactive)
  (let ((old-cwd default-directory))
    (while (not (or
               (string= (expand-file-name default-directory) "/")
               (file-exists-p "configure.ac")
               (file-exists-p "configure.in")))
      (cd ".."))
    (if (not (string= (expand-file-name default-directory) "/"))
        (when (not (= 0 (call-process "gtags" nil nil nil)))
          (message "error while creating tagfile"))
      (message "no suitable directory found for tagging"))
    (cd old-cwd)))

(defun negval/c-mode-common ()
  (interactive)
  (c-add-style "negval" negval/c-style)
  (c-add-style "mfisoft" mfisoft:c-style)

  (local-set-key (kbd "M-]") 'gtags-find-tag-from-here)

  (cond
   (+work-p+ (c-set-style "mfisoft" nil))
   (t        (c-set-style "negval" nil)))

  ;; highlight some stuff; ; this is for _all_ c modes
  (font-lock-add-keywords nil
                          '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):"
                             1 font-lock-warning-face prepend)))

  ;; highlight some stuff; this is for _all_ c modes
  (font-lock-add-keywords nil
                          '(("\\<\\(__\\(PRETTY_\\)?FUNCTION__\\|__LINE__\\|__FILE__\\)"
                             1 font-lock-preprocessor-face prepend)))
  (setq
   compilation-scroll-output 'first-error  ; scroll until first error
   compilation-read-command nil            ; don't need enter
   compilation-window-height 16            ; keep it readable
   c-hungry-delete-key t                   ; eat as much as possible
   ;; guess the identation of the current file, and use
   c++-font-lock-extra-types (quote ("JBF[a-zA-Z0-9]*" "Q[a-zA-Z]*"
                                     "u?int\\([1-9]+_t\\)?"
                                     "\\(::\\)?\\(std\\|boost\\)::[0-9a-zA-Z:_]+")))

  ;; that instead of my own settings; nice for foreign
  ;; files
  ;; https://savannah.nongnu.org/projects/dtrt-indent/
  (when (require-maybe 'dtrt-indent) (dtrt-indent-mode t))

  (when (require-maybe 'doxymacs)
    (doxymacs-mode t)
    (doxymacs-font-lock))

  (local-set-key (kbd "C-c i s") 'include-separator)
  (local-set-key (kbd "C-c i t") 'include-timestamp)
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
  (local-set-key (kbd "RET") 'newline-and-indent)

  ;; default .h files to C++
  (setq auto-mode-alist (append (list '("\\.h\\'" . c++-mode)) auto-mode-alist))

  (cond
   (+home-p+
    (local-set-key (kbd "C-c i h") 'include-header)
    (local-set-key (kbd "C-c i g") 'include-guards))
   (+work-p+
    (local-set-key (kbd "C-c i h") 'mfisoft:include-header)
    (local-set-key (kbd "C-c i g") 'mfisoft:include-guards)))

  ;; warn when lines are > 80 characters (in c-mode)
  (font-lock-add-keywords 'c-mode
                          '(("^[^\n]\\{80\\}\\(.*\\)$"
                             1 font-lock-warning-face prepend))))

(defun negval/c++-mode ()
  (when-available 'c-subword-mode
    (c-subword-mode))
  ;; warn when lines are > 100 characters (in c++-mode)
  (font-lock-add-keywords 'c++-mode
                          '(("^[^\n]\\{100\\}\\(.*\\)$"
                             1 font-lock-warning-face prepend)))
  (setq
   c-macro-preprocessor "cpp -x c++ `__dir=/usr/local/include; if [ -d $__dir ]; then echo -n -I$__dir; fi` -C")
  )

;; run befor all c-mode flavours
(add-hook 'c-mode-common-hook 'negval/c-mode-common)
;; run before c mode
;;(add-hook 'c-mode-hook 'negval/c-mode)
;; run before c++ mode
(add-hook 'c++-mode-hook 'negval/c++-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Makefiles
(defun negval/makefile-mode-hook ()
  (interactive)
  (setq show-trailing-whitespace t
        ident-tabs-mode nil))
(add-hook 'makefile-mode-hook 'negval/makefile-mode-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CMake
(autoload 'cmake-mode "cmake-mode" t)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ruby
(defun negval/ruby-mode-hook ()
  (ruby-electric-mode)
  (setq ruby-indent-level 2))

(add-hook 'ruby-mode-hook 'negval/ruby-mode-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell
(defun negval/browse-apropos-url (prefix prompt)
  (interactive)
  (let* ((thing (region-or-word-at-point)))
    (setq thing (read-string (format prompt thing) nil nil thing))
    (browse-apropos-url (concat prefix " " thing))))

(when (require-maybe 'inf-haskell)
  (setq auto-mode-alist
        (append auto-mode-alist
                '(("\\.[hg]s$"  . haskell-mode)
                  ("\\.hi$"     . haskell-mode)
                  ("\\.l[hg]s$" . literate-haskell-mode)))
        haskell-font-lock-symbols t)
  (autoload 'haskell-mode "haskell-mode"
    "Major mode for editing Haskell scripts." t)
  (autoload 'literate-haskell-mode "haskell-mode"
    "Major mode for editing literate Haskell scripts." t)
  (add-hook 'haskell-mode-hook
            (lambda nil
              (turn-on-haskell-decl-scan)
              (turn-on-haskell-doc-mode)
              (turn-on-haskell-indent)
              (turn-on-font-lock)
              (turn-on-eldoc-mode)))

  (add-hook 'haskell-mode-hook
            (lambda nil
              (local-set-key (kbd "C-c h") 'haskell-hoogle)
              (local-set-key (kbd "C-c C-h") 'haskell-hayoo))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell

;; Use ANSI colors within shell-mode.
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; syntax check
(defun sh-check-finish-hook (buf msg)
  "Function, that is executed at the end of sh check"
  (when (not (string-match "finished" msg))
    (next-error 1 t)))

(define-compilation-mode sh-check-mode "SH"
  "Mode for check sh source code."
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-scroll-output) nil)
  (set (make-local-variable 'compilation-finish-functions)
       (list 'sh-check-finish-hook)))

(defun sh-check-syntax ()
  "Check syntax of current file"
  (interactive)
  (when (string-match "^\\(ba\\|z\\)sh" (symbol-name sh-shell))
    (save-some-buffers t)
    (compilation-start (concat (symbol-name sh-shell) " -n " (buffer-file-name))
                       'sh-check-mode)))

(add-hook 'shell-script-mode-hook
          (lambda nil (local-set-key (kbd "C-c l") 'sh-check-syntax)))
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
;; flymake
(when (require-maybe 'flymake)
  (push '("\\.cxx\\'" flymake-simple-make-init) flymake-allowed-file-name-masks)
  (push '("\\.cc\\'"  flymake-simple-make-init) flymake-allowed-file-name-masks)
  (custom-set-faces
   '(flymake-errline ((((class color)) (:underline "Red2"))))
   '(flymake-warnline ((((class color)) (:underline "Blue2")))))
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (flymake-can-syntax-check-file buffer-file-name)
                (local-set-key (kbd "C-c C-v")
                               (lambda ()
                                 (interactive)
                                 (flymake-mode t)
                                 (flymake-goto-next-error)))
                (local-set-key (kbd "C-c v")
                               (lambda ()
                                 (interactive)
                                 (flymake-mode t)
                                 (flymake-goto-next-error)
                                 (flymake-display-err-menu-for-current-line))))))
  )
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
;; Org
(custom-set-variables
 '(org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "STARTED(s)" "|" "DONE(d)"
                                 "CANCELED(c)")))
 '(org-special-ctrl-a/e t)
 '(mark-diary-entries-in-calendar t)
 '(diary-file "~/documents/org/diary")
 '(org-agenda-files (quote ("~/documents/org/programming.org")))
 '(org-default-notes-file "~/documents/org/notes.org")
 '(org-directory "~/documents/org"))

(require 'org-install)
(require 'org)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'file-coding-system-alist (cons "\\.org$" 'utf-8))

(add-hook 'org-mode-hook 'turn-on-font-lock)

;; diary setup
(require 'diary-lib)
(add-hook 'diary-display-hook 'fancy-diary-display)

;; link abbrevs
(add-to-list 'org-link-abbrev-alist '("emacswiki" . "http://www.emacswiki.org/cgi-bin/wiki/"))
(add-to-list 'org-link-abbrev-alist '("google" . "http://www.google.com/search?q="))

;; remember mode
(org-remember-insinuate)
(define-key global-map (kbd "C-c r") 'org-remember)

(setq org-remember-templates
      '(
        ("Todo" ?t "* TODO %?\n %i\n %a" (concat org-directory "/TODO.org") "Tasks")
        ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git
(when (require-maybe 'magit)
  (custom-set-faces
   '(magit-item-highlight ((((class color) (background dark))
                            (:background "gray10"))))
   '(magit-log-tag-label ((((class color) (background dark))
                           (:background "gray15" :foreground "goldenrod1"))))
   '(magit-log-head-label ((((class color) (background dark))
                            (:background "gray15" :foreground "green yellow")))))
  (global-set-key (kbd "C-c m") 'magit-status))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CEDET
;; http://xtalk.msk.su/~ott/ru/writings/emacs-devenv/EmacsCedet.html
;;
;; Enabling Semantic (code-parsing, smart completion) features.
(load-file "~/.emacs.d/elisp/cedet/common/cedet.el")
(setq semantic-load-turn-useful-things-on t)
(semantic-load-enable-excessive-code-helpers)
;;(semantic-load-enable-semantic-debugging-helpers)

(setq
 senator-minor-mode-name "SN"
 semantic-imenu-auto-rebuild-directory-indexes nil)

(global-srecode-minor-mode 1)
(global-semantic-mru-bookmark-mode 1)
(global-semantic-folding-mode 1)      ; hide and show blocks
(global-semantic-idle-tag-highlight-mode 1)

(require 'semantic-decorate-include)
(require 'semantic-ia)
(require 'semantic-tag)

(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(setq-mode-local c++-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(setq-mode-local erlang-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
;;(setq-mode-local c++-mode semanticdb-implied-include-tags
;;                 (semantic-find-tags-included "/home/dsuhonin/projects/idfs/include/pch.hxx"))

;; Enable SemanticDB.
(require 'semanticdb)
;; TODO: parse `gcc -print-prog-name=cc1plus` -v < /dev/null
(require 'semantic-gcc)
(semantic-gcc-setup)
(global-semanticdb-minor-mode 1)
(setq semanticdb-default-save-directory "~/.tmp/semanticdb")
(when (not (file-exists-p semanticdb-default-save-directory))
  (make-directory semanticdb-default-save-directory))
;; (dolist (dir '("/usr/include" "/usr/local/include" "/usr/include/postgresql"
;;                "/usr/include/c++/4.2" "/usr/include/c++/4.2/backwards"
;;                "/usr/include/c++/4.3" "/usr/include/c++/4.3/backwards"))
;;   (when (file-exists-p dir)
;;     (semantic-add-system-include dir 'c-mode)
;;     (semantic-add-system-include dir 'c++-mode)))
(setq boost-dir "/usr/include/boost")
(when (file-exists-p boost-dir)
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file
               (concat boost-dir "/boost/config.hpp")))

;; EAssist.
(when (require-maybe 'eassist)
  (setq eassist-header-switches
        (append eassist-header-switches
                '(("hxx" "cxx") ("cxx" "hxx")
                  ("H" "cc") ("cc" "H"))
                )))

;; SpeedBar.
(require 'semantic-sb)

;; EDE.
(require 'semantic-lex-spp)
(global-ede-mode 1)

;; Enable Imenu.
(add-hook 'semantic-init-hooks
  (lambda ()
    (imenu-add-to-menubar "TAGS")))

(defun negval/cedet-hook nil
  (local-set-key (kbd "C-<return>") 'semantic-ia-complete-symbol)
  (local-set-key (kbd "C-c ?") 'semantic-ia-complete-symbol-menu)
  (local-set-key (kbd "C-c >") 'semantic-complete-analyze-inline)
  (local-set-key (kbd "C-c =") 'semantic-decoration-include-visit)
  (local-set-key (kbd "C-c p") 'semantic-analyze-proto-impl-toggle)
  (local-set-key (kbd "C-c j") 'semantic-ia-fast-jump)
  (local-set-key (kbd "C-c q") 'semantic-ia-show-doc)
  (local-set-key (kbd "C-c s") 'semantic-ia-show-summary)
  (local-set-key (kbd "C-c h") 'eassist-switch-h-cpp))

(add-hook 'c-mode-common-hook 'negval/cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'negval/cedet-hook)
(add-hook 'erlang-mode-hook 'negval/cedet-hook)

;; gnu global support
;; (require 'semanticdb-global)
;; (semanticdb-enable-gnu-global-databases 'c-mode)
;; (semanticdb-enable-gnu-global-databases 'c++-mode)

;; ctags
;; (require 'semanticdb-ectag)
;; (semantic-load-enable-primary-exuberent-ctags-support)

(custom-set-variables
 '(semantic-idle-scheduler-work-idle-time 10)
 '(semantic-self-insert-show-completion-function
   (lambda nil (semantic-ia-complete-symbol-menu (point))))
 '(global-semantic-tag-folding-mode
   t nil (semantic-util-modes)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ECB
(add-hook 'c-mode-common-hook
          (lambda nil (require-maybe 'ecb-autoloads)))
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
;; Some useful procedures.

;; Convert a buffer from dos ^M end of lines to unix end of lines.
(defun dos2unix ()
  (interactive)
    (goto-char (point-min))
      (while (search-forward "\r" nil t) (replace-match "")))
;; vice versa
(defun unix2dos ()
  (interactive)
    (goto-char (point-min))
      (while (search-forward "\n" nil t) (replace-match "\r\n")))
;; Insert current date.
(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%a %b %e, %Y %l:%M %p")))
;; Show ascii table.
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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:foreground "Green"))) t)
 '(diff-changed ((t (:foreground "MediumBlue"))) t)
 '(diff-context ((t (:foreground "White"))) t)
 '(diff-file-header ((t (:foreground "Red" :background "LightGray"))) t)
 '(diff-header ((t (:foreground "Red"))) t)
 '(diff-hunk-header ((t (:foreground "White" :background "Salmon"))) t)
 '(diff-index ((t (:foreground "Green"))) t)
 '(diff-nonexistent ((t (:foreground "Blue"))) t)
 '(diff-removed ((t (:foreground "Magenta"))) t)
 '(variable-pitch
   ((((class color)) (:family "Droid Sans"))))
 )

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

;;; general.el ---
;;; Copyright (c) 2009, Denis Sukhonin <d.sukhonin@gmail.com>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general settings

(set-language-environment "UTF-8") ; prefer utf-8 for language settings

;; don't show startup messages
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; turns off blink cursor
(when-available 'blink-cursor-mode
  (blink-cursor-mode 0))

(transient-mark-mode t)          ; make the current 'selection' visible
(delete-selection-mode t)        ; delete the selection area with a key press

(setq search-highlight t         ; highlight when searching...
      query-replace-highlight t) ; ...and replacing

(fset 'yes-or-no-p 'y-or-n-p)    ; enable one letter y/n answers to yes/no

(defalias 'qrr 'query-replace-regexp) ; search and replace by regular expression

(global-font-lock-mode t)         ; always do syntax highlighting
(when (require-maybe 'jit-lock)   ; enable JIT to make font-lock faster
  (setq jit-lock-stealth-time 1)) ; new with emacs21

;; the autosave is typically done by keystrokes, but I'd like to save
;; after a certain amount of time as well
(setq auto-save-timeout 1800)

(setq scroll-conservatively 10000)  ; smooth scrolling

(setq completion-ignore-case t      ; ignore case when completing...
      read-file-name-completion-ignore-case t) ; ...filenames too
(icomplete-mode t)

(put 'narrow-to-region 'disabled nil) ; enable...
(put 'erase-buffer 'disabled nil)     ; ... useful things

(when-available 'file-name-shadow-mode  ; emacs22+
  (file-name-shadow-mode 1))  ; be smart about filenames (understand ~/ etc.)

;; "don't hscroll unless needed"- ? More voodoo lisp.
(setq hscroll-margin 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define dirs for cacheing file dirs
;; see http://www.emacswiki.org/cgi-bin/wiki/FileNameCache
;; for more tricks with this...
(when-available 'file-cache-add-directory   ; emacs 22+
  (progn
    (defvar cachedirs
      '("~/" "/etc/"))
    (dolist (dir cachedirs) (file-cache-add-directory dir))))
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
;; the file-coding-system-alist
(add-to-list 'file-coding-system-alist (cons "/core400/" 'koi8-r))
(add-to-list 'file-coding-system-alist (cons "/db_agent/" 'koi8-r))
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

;; files without terminating newlines really annoy me
(setq require-final-newline t)

;; editing .xpi files directly in Emacs
(setq auto-mode-alist (cons '("\\.xpi$" . archive-mode) auto-mode-alist))


;;; general.el ends here

;; don't show startup messages
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; turns off blink cursor
(when-available 'blink-cursor-mode
                (blink-cursor-mode 0))

;; highlight parentheses
(show-paren-mode 1)

;; set mouse cursor color
(set-mouse-color "white")

;; mac switch meta key
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)

;; disable tab indentation by default
(setq tab-width 4)
(setq standard-indent 4)
(setq-default indent-tabs-mode nil)

;; auto reload buffer
;; don't annoy after git stash and checkouts
(global-auto-revert-mode t)

;; overwrite highlighted text
(delete-selection-mode 1)

;; enable one letter y/n answers to yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; manage backups
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 2
   kept-old-versions 2
   version-control t)

;; enable JIT to make font-lock faster
(require 'jit-lock)
(setq jit-lock-stealth-time 1)

;; show line and column numbers
(line-number-mode t)
(column-number-mode t)

;; show file size
(when-available 'size-indication-mode
                (size-indication-mode t))

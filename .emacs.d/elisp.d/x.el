;;; x.el ---
;; Copyright (c) 2009, Denis Sukhonin <d.sukhonin@gmail.com>

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
                 (:background "dark olive green" :foreground "light cyan"))))
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
                   :foreground "cyan1"))))
   '(diff-added ((t (:foreground "Green"))) t)
   '(diff-changed ((t (:foreground "MediumBlue"))) t)
   '(diff-context ((t (:foreground "White"))) t)
   '(diff-file-header ((t (:foreground "Red" :background "LightGray"))) t)
   '(diff-header ((t (:foreground "Red"))) t)
   '(diff-hunk-header ((t (:foreground "White" :background "Salmon"))) t)
   '(diff-index ((t (:foreground "Green"))) t)
   '(diff-nonexistent ((t (:foreground "Blue"))) t)
   '(diff-removed ((t (:foreground "Magenta"))) t)
   '(variable-pitch ((((class color)) (:family "Droid Sans"))))
   '(flymake-errline ((((class color)) (:underline "Red2"))))
   '(flymake-warnline ((((class color)) (:underline "Blue2"))))
   ))

(defun negval/x-bars nil
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0))

(negval/x-clipboard)
(negval/x-colors)
(negval/x-bars)

;;; x.el ends here

;;; x.el ---
;; Copyright (c) 2009, Denis Sukhonin <d.sukhonin@gmail.com>

(require 'color-theme)

(defun negval/x-clipboard nil
  (setq x-select-enable-clipboard nil)      ; copy-paste should work ...
  (setq interprogram-paste-function         ; ...with...
        'x-cut-buffer-or-selection-value))  ; ...other X clients

(defun negval/x-bars nil
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0))

(defun negval/x-etc nil
  ;; show-paren-mode
  ;; show a subtle blinking of the matching paren (the defaults are ugly)
  ;; http://www.emacswiki.org/cgi-bin/wiki/ShowParenMode
  (when-available 'show-paren-mode
    (progn
      (setq show-paren-style 'expression)
      (show-paren-mode t)))

  ;; highlight the current line; set a custom face, so we can
  ;; recognize from the normal marking (selection)
  ;; don't turn in on globally, only in specific modes (see negval/c-mode-hook)
  (when-available 'global-hl-line-mode
    (progn
      (setq hl-line-face 'hl-line)
      (global-hl-line-mode t))) ;; turn it on for all modes by default

  (when-available 'set-fringe-mode    ; emacs22+
    (progn
      (set-fringe-mode '(nil . 0))
      (setq indicate-buffer-boundaries 'left)
      ))

  (mouse-wheel-mode t)               ; turn on mouse's wheel
  )

(defvar negval/*cursor-color*)
(defvar negval/*cursor-color-ro*)
(defvar negval/*background-color*)
(defvar negval/*foreground-color*)

(defun negval/x-dark-theme ()
  "Dark blue color theme by Denis Sukhonin."
  (interactive)

  (setq negval/*cursor-color*     "DodgerBlue2")
  (setq negval/*cursor-color-ro*  "White")
  (setq negval/*background-color* "Black")
  (setq negval/*foreground-color* "DodgerBlue1")

  (let ((face-req '((class color) (background dark))))
    (color-theme-install
     `(negval/x-dark-theme
       ((background-color . "Black")
        (background-mode  . dark)
        (border-color     . "DodgerBlue4")
        (cursor-color     . ,negval/*cursor-color*)
        (foreground-color . "DodgerBlue1")
        (mouse-color      . "DodgerBlue2"))
       ((hl-line-face . hl-line))
       (sans           ((,face-req (:family "Droid Sans"))))
       (serif          ((,face-req (:family "Droid Serif"))))
       (variable-pitch ((,face-req (:inherit sans))))
       (fixed-pitch    ((,face-req (:family "Monaco"))))
       (bold           ((,face-req (:bold t :weight bold))))
       (bold-italic    ((,face-req (:italic t :bold t :slant italic :weight bold))))
       (italic         ((,face-req (:italic t :slant italic))))
       (small          ((,face-req (:height 0.85))))
       (default        ((,face-req (:inherit fixed-pitch :height 80))))

       (fringe         ((,face-req (:background "grey10"))))
       (highlight      ((,face-req (:foreground "orange" :underline "orange" :inherit bold))))
       (region         ((,face-req (:background "midnight blue"))))
       (match          ((,face-req (:background "midnight blue"))))
       (lazy-highlight ((,face-req (:background "midnight blue"))))
       (hl-line        ((,face-req (:background "gray5"))))
       (mode-line      ((,face-req (:background "gray10" :foreground "gray90"
                                    :inverse-video nil :box nil
                                    :inherit (sans small)))))
       (mode-line-buffer-id ((,face-req (:foreground "green"))))
       (mode-line-emphasis  ((,face-req (:inherit bold))))
       (mode-line-highlight ((,face-req (:inherit highlight))))
       (mode-line-inactive  ((,face-req (:background "gray7" :foreground "gray90"
                                         :inverse-video nil :box nil
                                         :family "Droid Sans" :height 0.85 :width condensed))))
       (modeline-mousable            ((,face-req (:box (:line-width 1 :style none)))))
       (modeline-mousable-minor-mode ((,face-req (:box (:line-width 1 :style none)))))
       (which-func ((,face-req (:foreground "lime green" :width condensed :inherit fixed-pitch))))
       (border ((,face-req (:background "DodgerBlue4"))))
       (show-paren-match    ((,face-req (:background "gray10"))))
       (show-paren-mismatch ((,face-req (:foreground "Red" :inherit bold))))
       (header-line ((,face-req (:background "gray10" :inherit (fixed-pitch)))))
       (flymake-errline    ((,face-req (:underline "Red2"  :inherit bold))))
       (flymake-warnline   ((,face-req (:underline "Gold3" :inherit bold))))
       (flyspell-incorrect ((,face-req (:underline "Red2"))))
       (flyspell-duplicate ((,face-req (:underline "Gold3"))))
       (linum ((,face-req (:foreground "Green" :inherit (fringe)))))

       (font-lock-builtin-face           ((,face-req (:foreground "LightSteelBlue"))))
       (font-lock-comment-delimiter-face ((,face-req (:foreground "Tan3"))))
       (font-lock-comment-face           ((,face-req (:foreground "Tan1"))))
       (font-lock-constant-face          ((,face-req (:foreground "Aquamarine"))))
       (font-lock-doc-face               ((,face-req (:foreground "LightSalmon" :background "Gray10"))))
       (font-lock-doc-string-face        ((,face-req (:foreground "LightSalmon" :background "Gray10"))))
       (font-lock-function-name-face     ((,face-req (:foreground "LightSkyBlue" :inherit bold))))
       (font-lock-keyword-face           ((,face-req (:foreground "Cyan"))))
       (font-lock-preprocessor-face      ((,face-req (:foreground "Aquamarine"))))
       (font-lock-reference-face         ((,face-req (:foreground "LightSteelBlue"))))
       (font-lock-string-face            ((,face-req (:foreground "LightSalmon"))))
       (font-lock-type-face              ((,face-req (:foreground "PaleGreen" :inherit bold))))
       (font-lock-variable-name-face     ((,face-req (:foreground "LightGoldenrod"))))
       (font-lock-warning-face           ((,face-req (:bold t :foreground "Pink" :weight bold))))

       (widget-button            ((,face-req (:bold t :weight bold))))
       (widget-button-pressed    ((,face-req (:foreground "red"))))
       (widget-documentation     ((,face-req (:inherit font-lock-comment-face))))
       (widget-field             ((,face-req (:background "gray10" :box t))))
       (widget-inactive          ((,face-req (:foreground "light gray"))))
       (widget-single-line-field ((,face-req (:background "gray10" :box t))))

       (toolbar               ((,face-req (:background "DodgerBlue2" :foreground "white"
                                           :box (:line-width 1 :color "DodgerBlue1" :style released-button)))))
       (custom-button         ((,face-req (:background "DodgerBlue4" :foreground "white"
                                           :box (:line-width 1 :color "DodgerBlue4" :style released-button)))))
       (custom-button-mouse   ((,face-req (:background "DodgerBlue3" :inherit custom-button))))
       (custom-button-pressed ((,face-req (:background "DodgerBlue4" :foreground "White"
                                           :box (:line-width 1 :color "DodgerBlue4" :style pressed-button)))))

       (dired-directory ((,face-req (:foreground "LightSkyBlue" :inherit bold))))
       (dired-symlink   ((,face-req (:foreground "Cyan"))))

       (diff-added             ((,face-req (:foreground "Green1"))))
       (diff-changed           ((,face-req (:foreground "Light Slate Blue"))))
       (diff-context           ((,face-req (:foreground "White"))))
       (diff-file-header       ((,face-req (:background "LightGray" :foreground "Red4"))))
       (diff-file1-hunk-header ((,face-req (:background "DarkSeaGreen1" :foreground "Blue"))))
       (diff-file2-hunk-header ((,face-req (:background "PaleGoldenrod" :foreground "Red"))))
       (diff-function          ((,face-req (:foreground "Red"))))
       (diff-header            ((,face-req (:foreground "DodgerBlue2"))))
       (diff-hunk-header       ((,face-req (:background "Salmon" :foreground "Blue"))))
       (diff-index             ((,face-req (:foreground "Green"))))
       (diff-indicator-added   ((,face-req (:background "Green4" :foreground "PaleGoldenrod"))))
       (diff-indicator-changed ((,face-req (:background "Dark Slate Blue" :foreground "PaleGoldenrod"))))
       (diff-indicator-removed ((,face-req (:background "Firebrick4" :foreground "PaleGoldenrod"))))
       (diff-nonexistent       ((,face-req (:foreground "Blue"))))
       (diff-refine-change     ((,face-req (:background nil :box t))))
       (diff-removed           ((,face-req (:foreground "Firebrick1"))))
       ;; jabber-mode
       (jabber-roster-user-online ((,face-req (:foreground "White"))))
       (jabber-roster-user-away   ((,face-req (:foreground "Gray40"))))
       (jabber-roster-user-xa     ((,face-req (:foreground "Dark Violet"))))
       (jabber-title-small        ((,face-req (:foreground "Dodger Blue" :height 1.3 :underline t
                                               :inherit (variable-pitch bold)))))
       (jabber-title-medium       ((,face-req (:foreground "Dodger Blue" :height 2.0 :underline t
                                               :inherit (variable-pitch bold)))))
       (jabber-title-large        ((,face-req (:foreground "Dodger Blue" :height 3.0 :underline t
                                               :inherit (variable-pitch bold)))))
       ;; slime
       (slime-highlight-edits-face ((,face-req (:background "Midnight Blue"))))
       ;; undo-tree
       (undo-tree-visualizer-active-branch-face ((,face-req (:foreground "Green1" :inherit bold))))
       (undo-tree-visualizer-current-face       ((,face-req (:foreground ,negval/*cursor-color* :inherit bold))))
       ))))

(add-to-list 'color-themes '(negval/x-dark-theme  "My dark theme" "Denis Sukhonin"))

(defun negval/x-snowish-theme ()
  "Winter-time color theme by Denis Sukhonin."
  (interactive)

  (setq negval/*cursor-color*     "LightSteelBlue4")
  (setq negval/*cursor-color-ro*  "Black")
  (setq negval/*background-color* "Snow1")
  (setq negval/*foreground-color* "Dark Slate Gray")

  (let ((face-req '((class color) (background light))))
    (color-theme-install
     `(negval/x-dark-theme
       ((background-color . ,negval/*background-color*)
        (background-mode  . light)
        (border-color     . ,negval/*background-color*)
        (cursor-color     . ,negval/*cursor-color*)
        (foreground-color . ,negval/*foreground-color*)
        (mouse-color      . ,negval/*cursor-color*))
       ;; variables
       ((hl-line-face . hl-line)
        )
       ;; faces
       (sans           ((t (:family "Droid Sans"))))
       (serif          ((t (:family "Droid Serif"))))
       (variable-pitch ((t (:inherit sans))))
       (fixed-pitch    ((t (:family "Monaco"))))
       (bold           ((t (:bold t :weight bold))))
       (bold-italic    ((t (:italic t :bold t :slant italic :weight bold))))
       (italic         ((t (:italic t :slant italic))))
       (small          ((t (:height 0.85))))
       (default        ((t (:inherit fixed-pitch :height 80))))

       (fringe    ((t (:background "Snow2"))))
       (highlight ((t (:foreground "Orange3" :underline "Orange3" :inherit bold))))
       (region    ((t (:background "Moccasin"))))
       (hl-line   ((t (:background "Mint Cream"))))
       (mode-line ((t (:background "Snow3" :foreground "Gray10"
                       :inverse-video nil :box nil
                       :inherit (sans small)))))
       (mode-line-buffer-id ((t (:foreground "Blue4"))))
       (mode-line-emphasis  ((t (:inherit bold))))
       (mode-line-highlight ((t (:inherit highlight))))
       (mode-line-inactive  ((t (:background "Snow2" :foreground "gray10"
                                 :inverse-video nil :box nil
                                 :family "Droid Sans" :height 0.85 :width condensed))))
       (modeline-mousable            ((t (:box (:line-width 1 :style none)))))
       (modeline-mousable-minor-mode ((t (:box (:line-width 1 :style none)))))
       (which-func ((t (:foreground "Blue4" :width condensed :inherit fixed-pitch))))
       (show-paren-match    ((t (:background "Alice Blue"))))
       (show-paren-mismatch ((t (:foreground "Red" :inherit bold))))
       (header-line ((t (:background "Snow2" :inherit (fixed-pitch)))))
       (flymake-errline    ((t (:underline "Red2"  :inherit bold))))
       (flymake-warnline   ((t (:underline "Gold3" :inherit bold))))
       (flyspell-incorrect ((t (:underline "Red2"))))
       (flyspell-duplicate ((t (:underline "Gold3"))))
       (linum ((t (:foreground "DeepSkyBlue4" :inherit (fringe)))))

       (font-lock-builtin-face           ((t (:foreground "Blue"))))
       (font-lock-comment-delimiter-face ((t (:foreground "DeepSkyBlue1"))))
       (font-lock-comment-face           ((t (:foreground "DeepSkyBlue3"))))
       (font-lock-constant-face          ((t (:foreground "CadetBlue4"))))
       (font-lock-doc-face               ((t (:foreground "MediumBlue" :background "White Smoke"))))
       (font-lock-doc-string-face        ((t (:foreground "MediumBlue" :background "White Smoke"))))
       (font-lock-function-name-face     ((t (:foreground "DarkBlue" :inherit bold))))
       (font-lock-keyword-face           ((t (:foreground "Blue4"))))
       (font-lock-preprocessor-face      ((t (:foreground "Blue3"))))
       (font-lock-reference-face         ((t (:foreground "Red3"))))
       (font-lock-string-face            ((t (:foreground "DarkViolet"))))
       (font-lock-type-face              ((t (:foreground "GoldenRod" :inherit bold))))
       (font-lock-variable-name-face     ((t (:foreground "Tomato"))))
       (font-lock-warning-face           ((t (:foreground "Red" :inherit bold))))

       (widget-button            ((t (:bold t :weight bold))))
       (widget-button-pressed    ((t (:foreground "Red"))))
       (widget-documentation     ((t (:inherit font-lock-comment-face))))
       (widget-field             ((t (:background "Gray85" :box t))))
       (widget-inactive          ((t (:foreground "Light Gray"))))
       (widget-single-line-field ((t (:background "Gray85" :box t))))

       (toolbar               ((t (:background "Gray90" :foreground "Black"
                                   :box (:line-width 1 :color "Gray90" :style released-button)))))
       (custom-button         ((t (:background "Gray85" :foreground "Black"
                                   :box (:line-width 1 :color "Gray85" :style released-button)))))
       (custom-button-mouse   ((t (:background "Gray90" :inherit custom-button))))
       (custom-button-pressed ((t (:background "Gray85" :foreground "Black"
                                   :box (:line-width 1 :color "Gray85" :style pressed-button)))))

       (dired-directory ((t (:foreground "Blue4" :inherit bold))))
       (dired-symlink   ((t (:foreground "Dark Cyan"))))

       (diff-added             ((t (:foreground "Green4"))))
       (diff-changed           ((t (:foreground "Light Slate Blue"))))
       (diff-context           ((t (:foreground ,negval/*foreground-color*))))
       (diff-file-header       ((t (:background "LightGray" :foreground "Red4"))))
       (diff-file1-hunk-header ((t (:background "DarkSeaGreen1" :foreground "Blue"))))
       (diff-file2-hunk-header ((t (:background "PaleGoldenrod" :foreground "Red"))))
       (diff-function          ((t (:foreground "Red"))))
       (diff-header            ((t (:foreground "DodgerBlue4"))))
       (diff-hunk-header       ((t (:background "Salmon" :foreground "Blue"))))
       (diff-index             ((t (:foreground "Green"))))
       (diff-indicator-added   ((t (:background "Green4" :foreground "PaleGoldenrod"))))
       (diff-indicator-changed ((t (:background "Dark Slate Blue" :foreground "PaleGoldenrod"))))
       (diff-indicator-removed ((t (:background "Firebrick4" :foreground "PaleGoldenrod"))))
       (diff-nonexistent       ((t (:foreground "Blue"))))
       (diff-refine-change     ((t (:background nil :box t))))
       (diff-removed           ((t (:foreground "Firebrick1"))))
       ;; undo-tree
       (undo-tree-visualizer-active-branch-face ((,face-req (:foreground "Green2" :inherit bold))))
       (undo-tree-visualizer-current-face       ((,face-req (:foreground ,negval/*cursor-color* :inherit bold))))
       ))))

(add-to-list 'color-themes '(negval/x-snowish-theme  "My snowish theme" "Denis Sukhonin"))

;; change cursor color based on mode
;; http://www.emacswiki.org/cgi-bin/wiki/download/cursor-chg.el
(setq hcz-set-cursor-color-color "")
(setq hcz-set-cursor-color-buffer "")
(defun hcz-set-cursor-color-according-to-mode ()
  "change cursor color according to some minor modes."
  ;; set-cursor-color is somewhat costly, so we only call it when
  ;; needed:
  (let ((color
         (if buffer-read-only negval/*cursor-color-ro*
           (if overwrite-mode "Red" negval/*cursor-color*))))
    (unless (and
             (string= color hcz-set-cursor-color-color)
             (string= (buffer-name) hcz-set-cursor-color-buffer))
      (set-cursor-color (setq hcz-set-cursor-color-color color))
      (setq hcz-set-cursor-color-buffer (buffer-name)))))

(negval/x-clipboard)
(negval/x-dark-theme)
(negval/x-bars)
(negval/x-etc)

(add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode)

;;; x.el ends here

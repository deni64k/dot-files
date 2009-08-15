;;; tex.el ---
;; Copyright (c) 2009, Denis Sukhonin <d.sukhonin@gmail.com>

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

;;; tex.el ends here

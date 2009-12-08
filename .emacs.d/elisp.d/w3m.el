(add-to-list 'load-path "~/.emacs.d/elisp/w3m")

(require 'w3m)

(autoload 'w3m "w3m" "Interface for w3m on Emacs." t)

(setq browse-url-browser-function 'w3m-browse-url
      w3m-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-output-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8
      w3m-use-cookies t)

(global-set-key (kbd "C-x m") 'browse-url-at-point)

(add-hook 'w3m-display-hook
          (lambda (url)
            (rename-buffer
             (format "*w3m: %s*" (or w3m-current-title
                                     w3m-current-url)) t)))

(defun w3m-open-current-page-in-firefox ()
  "Opens the current URL in Mozilla Firefox."
  (interactive)
  (browse-url-firefox w3m-current-url))

(defun w3m-open-link-or-image-in-firefox ()
  "Opens the current link or image in Firefox."
  (interactive)
  (browse-url-firefox (or (w3m-anchor)
                          (w3m-image))))

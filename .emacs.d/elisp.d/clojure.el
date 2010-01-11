;;; clojure.el ---
;;; Copyright (c) 2010, Denis Sukhonin <d.sukhonin@gmail.com>

(add-to-list 'load-path "~/.emacs.d/elisp/clojure-mode")
(require 'clojure-mode)

(defun negval/clojure-mode-hook ()
  (turn-on-eldoc-mode)
  (set-fill-column 100)
  (auto-fill-mode 1)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (set (make-local-variable 'slime-lisp-implementations)
       (list (assoc 'clojure slime-lisp-implementations))))

(add-hook 'clojure-mode-hook 'negval/clojure-mode-hook)

(setq clj-path             "~/src/clojure/"
      clj-jar-path         "~/src/clojure/clojure.jar"
      clj-contrib-jar-path "~/src/clojure-contrib/clojure-contrib.jar")

(setq swank-clojure-jar-path clj-jar-path
      swank-clojure-extra-classpaths (list
                                      clj-path
                                      clj-contrib-jar-path
                                      ))

(add-to-list 'load-path "~/.emacs.d/elisp/swank-clojure/src/emacs")

(require 'swank-clojure)
(add-hook 'slime-indentation-update-hooks 'swank-clojure-update-indentation)
(add-hook 'slime-repl-mode-hook 'swank-clojure-slime-repl-modify-syntax t)
(add-hook 'clojure-mode-hook 'swank-clojure-slime-mode-hook t)

;; taken from Bill Climenson config
(defun slime-java-describe (symbol-name)
  "Get details on Java class/instance at point."
  (interactive (list (slime-read-symbol-name "Java Class/instance: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (save-excursion
    (set-buffer (slime-output-buffer))
    (unless (eq (current-buffer) (window-buffer))
      (pop-to-buffer (current-buffer) t))
    (goto-char (point-max))
    (insert (concat "(clojure.contrib.repl-utils/show " symbol-name ")"))
    (when symbol-name
      (slime-repl-return)
      (other-window 1))))

(defun slime-javadoc (symbol-name)
  "Get JavaDoc documentation on Java class at point."
  (interactive (list (slime-read-symbol-name "JavaDoc info for: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (set-buffer (slime-output-buffer))
  (unless (eq (current-buffer) (window-buffer))
    (pop-to-buffer (current-buffer) t))
  (goto-char (point-max))
  (insert (concat "(clojure.contrib.javadoc/javadoc " symbol-name ")"))
  (when symbol-name
    (slime-repl-return)
    (other-window 1)))

(defun alexott/clojure-slime-conn-hook ()
  ;; (require 'clojure-mode)
  ;; (slime-redirect-inferior-output)
  (def-slime-selector-method ?j "most recently visited clojure-mode buffer."
    (slime-recently-visited-buffer 'clojure-mode))
  (define-key slime-mode-map (kbd "<return>") 'newline-and-indent)
  (define-key slime-mode-map (kbd "C-j") 'newline)
  (define-key slime-mode-map (kbd "C-c d") 'slime-java-describe)
  (define-key slime-repl-mode-map (kbd "C-c d") 'slime-java-describe)
  (define-key slime-mode-map (kbd "C-c D") 'slime-javadoc)
  (define-key slime-repl-mode-map (kbd "C-c D") 'slime-javadoc)
  )
(add-hook 'slime-connected-hook 'alexott/clojure-slime-conn-hook)

;;; clojure.el ends here

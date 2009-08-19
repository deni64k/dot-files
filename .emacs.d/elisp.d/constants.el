;;; constants.el ---
;; Copyright (c) 2009, Denis Sukhonin <d.sukhonin@gmail.com>

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

;;; constants.el ends here

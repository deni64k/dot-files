;;; emms.el ---

;; TODO: use emms-auto.el

(add-to-list 'load-path "~/.emacs.d/elisp/emms/lisp")
(require 'emms-player-simple)
(require 'emms-source-file)
(require 'emms-source-playlist)

(setq emms-repeat-playlist t
      emms-player-list '(emms-player-mpg321
                         emms-player-ogg123
                         emms-player-mplayer)
      emms-source-file-default-directory "~/music/")

(require 'emms-player-mpd)
(setq emms-player-mpd-server-name "localhost"
      emms-player-mpd-server-port "6600"
      emms-player-mpd-music-directory "~/music/"
      emms-volume-change-function 'emms-volume-mpd-change)
(add-to-list 'emms-info-functions 'emms-info-mpd)
(add-to-list 'emms-player-list 'emms-player-mpd)

(require 'emms-info-libtag)
(setq emms-info-functions '(emms-info-libtag))

(require 'emms-browser)
(global-set-key (kbd "<f2>") 'emms-smart-browse)

(global-set-key (kbd "C-c e w") 'emms-show)
(global-set-key (kbd "C-c e n") 'emms-next)
(global-set-key (kbd "C-c e p") 'emms-previous)
(global-set-key (kbd "C-c e s") 'emms-stop)
(global-set-key (kbd "C-c e r") 'emms-start)

;;; emms.el ends here

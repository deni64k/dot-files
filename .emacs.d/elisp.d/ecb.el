;;; ecb.el ---
;; Copyright (c) 2009, Denis Sukhonin <d.sukhonin@gmail.com>

(add-hook 'c-mode-common-hook
          (lambda nil (require-maybe 'ecb-autoloads)))

;; Эти три строки создают привязки активизации / деактивизации модуля
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

;;; ecb.el ends here

(add-to-list 'load-path "~/.emacs.d/elisp/distel/elisp")

(require 'distel)
(distel-setup)

(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))

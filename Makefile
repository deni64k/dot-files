## Makefile ---
## Copyright (c) 2010, Denis Sukhonin <d.sukhonin@gmail.com>

# TODO: easy syncing in both sides, show differences.

packages	= emacs git xmonad zsh
emacs_FILES	= .emacs .emacs.d
git_FILES	= .gitconfig
xmonad_FILES	= .xmonad
zsh_FILES	= .zshrc .zshenv .zsh

define package_TEMPLATE =
.PHONY: install-$(1)
install-$(1): $(addprefix $(HOME)/,$($(1)_FILES))

$(addprefix $(HOME)/,$($(1)_FILES)): $($(1)_FILES)
	if [ -d $$< ]; then
	  cp -vr $$< ~/
	else
	  cp -v $$< ~/
	fi
endef

$(foreach package,$(packages),$(eval $(call package_TEMPLATE,$(package))))

.PHONY: install install-all
install: install-all
install-all: $(addprefix install-,$(packages))

.PHONY: dump-var
dump-var:
	@echo '$($(VAR))'

## Makefile ends here

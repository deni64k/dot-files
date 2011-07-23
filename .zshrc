# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
export ZSH_THEME="gallois"

# Set to this to use case-sensitive completion
# export CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# export DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# export DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# export DISABLE_AUTO_TITLE="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(bundler cap gem github lein rails rails3 redis-cli ruby rvm git)
plugins+=(archlinux)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export PATH=/home/denis/bin:/home/denis/.cabal/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:/usr/share/java/apache-ant/bin:/opt/maven/bin:/usr/bin/vendor_perl:/usr/bin/core_perl:/home/denis/.rvm/bin

export PROMPT="%{$fg[blue]%}%D{%T} %{$fg[green]%}%m ${PROMPT}"

bindkey "^[Oc"  forward-word
bindkey "^[Od"  backward-word

alias df='df -h'

alias ll="ls -l"
alias ltr="ls -ltr"

if test "`awk '{ print \$1; }' <<< $EDITOR`" = "emacs"; then
    alias e="emacsclient -t"
    alias E="emacsclient -c"
else
    alias e="$EDITOR"
    alias E="$EDITOR"
fi

alias -g M="|$PAGER"
alias -g L="|$PAGER"
alias -g H='|head'
alias -g T='|tail'
alias -g N='2>/dev/null'

alias t-r="http_proxy= transmission-remote"

HISTFILE=~/.zhistory
SAVEHIST=5000
HISTSIZE=5000
DIRSTACKSIZE=20

setopt  APPEND_HISTORY
setopt  HIST_IGNORE_ALL_DUPS
setopt  HIST_IGNORE_SPACE
setopt  HIST_REDUCE_BLANKS

setopt  No_Beep
setopt  IGNORE_EOF

strerror() { perl -MPOSIX -e "print strerror($1).\"\\n\";" }

typeset -U path cdpath fpath

# Опции общего поведения
bindkey -e      # peжuм нaвuгaцuu в cтuлe emacs
bindkey ' ' magic-space    # also do history expansion on space
bindkey '^I' complete-word # complete on tab, leave expansion to _expand

## Setting up HOME, END and DELETE keys
case $TERM in
  linux)
    bindkey "^[[2~" yank
    bindkey "^[[3~" delete-char
    bindkey "^[[5~" up-line-or-history
    bindkey "^[[6~" down-line-or-history
    bindkey "^[[1~" beginning-of-line
    bindkey "^[[4~" end-of-line
    bindkey "^[e"   expand-cmd-path ## C-e for expanding path of typed command
    bindkey "^[[A"  up-line-or-search ## up arrow for back-history-search
    bindkey "^[[B"  down-line-or-search ## down arrow for fwd-history-search
    bindkey "^[d"   delete-word
  ;;
  *xterm*|rxvt|(dt|k|E)term)
    bindkey "^[[2~" yank
    bindkey "^[[3~" delete-char
    bindkey "^[[5~" up-line-or-history
    bindkey "^[[6~" down-line-or-history
    bindkey "^[[H"  beginning-of-line
    bindkey "^[[F"  end-of-line
    bindkey "^[e"   expand-cmd-path ## C-e for expanding path of typed command
    bindkey "^[[A"  up-line-or-search ## up arrow for back-history-search
    bindkey "^[[B"  down-line-or-search ## down arrow for fwd-history-search
    bindkey "^[d"   delete-word
  ;;
esac

alias mv='nocorrect mv'         # переименование-перемещение c пogтвepжgeнueм
alias cp='nocorrect cp -R'      # рекурсивное копирование с подтверждением
alias rm='nocorrect rm'         # удаление с подтверждением
alias rmf='nocorrect rm -f'     # принудимтельное удаление
alias rmrf='nocorrect rm -fR'   # принудительное рекурсивное удаление
alias mkdir='nocorrect mkdir'   # создание каталогов без коррекции

alias h=history
alias grep='fgrep'

alias df='df -h'

alias ispell='ispell -d russian'

# lsmods for listing of kernel modules
case `uname -s` in
    FreeBSD)
        alias lsmods="find /boot/kernel -name '*.ko'"
        ls='ls -G'
        ;;
    Linux)
        ls='ls --color=auto'
        alias lsmods="find /lib/modules/`uname -r` -name '*.ko'"
        alias gmake=make
        ;;
esac

alias l="${ls}"
alias ls="${ls}"
alias ll="${ls} -l"
alias la="${ls} -Ah"
alias li="${ls} -ial"
alias lsd="${ls} -ld *(-/DN)"
alias lsa="${ls} -ld .*"
unset ls

if test "`awk '{ print \$1; }' <<< $EDITOR`" = "emacs"; then
    alias e="emacsclient -c"
    alias E="emacsclient -t"
else
    alias e="$EDITOR"
    alias E="$EDITOR"
fi

alias -g M="|$PAGER"
alias -g L="|$PAGER"
alias -g H='|head'
alias -g T='|tail'
alias -g N='2>/dev/null'

alias t-r='http_proxy= transmission-remote'
alias mount='sudo mount'
alias umount='sudo umount'
alias boinc-cmd='boinc_cmd --passwd `sudo cat /var/db/boinc/gui_rpc_auth.cfg`'
alias boinc-cmd-h='boinc_cmd -h'

hosts=('hostname' freebsd.org openbsd.org openbsd.ru puffy dennis vladimir marina 192.168.0.)

# Всякие переменные

## файл истории команд
## если не указан, история не будет сохраняться
## при выходе из сеанса
HISTFILE=~/.zhistory

## Число команд, сохраняемых в HISTFILE
SAVEHIST=5000

## Чucлo koмaнg, coxpaняeмыx в сеансе
HISTSIZE=5000
## Примечание:
## рекомендуются равные значения для
## SAVEHIST и HISTSIZE

DIRSTACKSIZE=20

# Опции истории команд

## Дополнение файла истрии
setopt  APPEND_HISTORY

## Игнopupoвaть вce пoвтopeнuя команд
setopt  HIST_IGNORE_ALL_DUPS

## Игнopupoвать лишние пpoбeлы
setopt  HIST_IGNORE_SPACE

## Удалять из файл истории пустые строки
setopt  HIST_REDUCE_BLANKS

# Установка-снятие опций шелла
setopt   notify globdots correct pushdtohome cdablevars autolist
setopt   correctall autocd recexact longlistjobs
setopt   autoresume histignoredups pushdsilent noclobber
setopt   autopushd pushdminus extendedglob rcquotes mailwarning
unsetopt bgnice autoparamslash

## Отключение звукового сигнала
## при ошибках
setopt  No_Beep

## Нe cчuтaть Control+D зa выxog uз oбoлoчku
setopt  IGNORE_EOF

# Autoload zsh modules when they are referenced
zmodload -a  zsh/stat stat
zmodload -a  zsh/zpty zpty
zmodload -a  zsh/zprof zprof
zmodload -ap zsh/mapfile mapfile

# Для разворота сокращенного ввода типа cd d/e в docs/editors
autoload -U compinit
compinit

# Shell functions
setenv() { typeset -x "${1}${1:+=}${(@)argv[2,$#]}" }  # csh compatibility
freload() { while (( $# )); do; unfunction $1; autoload -U $1; shift; done }
strerror() { perl -MPOSIX -e "print strerror($1).\"\\n\";" }

# Where to look for autoloaded function definitions
fpath=($fpath ~/.zfunc)
fpath=($fpath $HOME/.zsh/functions)

# Autoload all shell functions from all directories in $fpath (following
# symlinks) that have the executable bit on (the executable bit is not
# necessary, but gives you an easy way to stop the autoloading of a
# particular shell function). $fpath should not be empty for this to work.
for func in $^fpath/*(N-.x:t); autoload $func

# automatically remove duplicates from these arrays
typeset -U path cdpath fpath manpath

# search path for cd
typeset -u cdpathc
cdpath=(.. ~)

# man path
typeset -u manpath
manpath=/(usr/local/man /usr/man /usr/share/man \
    /usr/local/share/man /usr/X11R6/man/)

################################################################################
# some one-time setup things
#
# create ~/.tmp and ~/.emacs.d/backups if it's not there yet
for __path in $HOME/.tmp $HOME/.emacs.d/backups; do
    test -e ${__file} || mkdir -p ${__file}
done
################################################################################

################################################################################
# Completion Styles
#
# list of completers to use
zstyle ':completion:*::::' completer _expand _complete _ignored _approximate

# allow one error for every three characters typed in approximate completer
zstyle -e ':completion:*:approximate:*' max-errors \
    'reply=( $(( ($#PREFIX+$#SUFFIX)/3 )) numeric )'

# insert all expansions for expand completer
zstyle ':completion:*:expand:*' tag-order all-expansions

# formatting and messages
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'
zstyle ':completion:*' group-name ''

# match uppercase from lowercase
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# don't autocomplete what's already on the line
zstyle ':completion:*:*' ignore-line yes

# offer indexes before parameters in subscripts
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

# command for process lists, the local web server details and host completion
zstyle ':completion:*:processes' command 'ps -o pid,s,nice,stime,args'
#zstyle ':completion:*:urls' local 'www' '/var/www/htdocs' 'public_html'
zstyle '*' hosts $hosts

# filename suffixes to ignore during completion (except after rm command)
zstyle ':completion:*:*:(^rm):*:*files' ignored-patterns '*?.o' '*?.c~' \
    '*?.old' '*?.pro' '*~' '#*'
# the same for old style completion
#fignore=(.o .c~ .old .pro)

# ignore completion functions (until the _ignored completer)
zstyle ':completion:*:functions' ignored-patterns '_*'

# use caching for the completions
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.tmp/zsh-cache
################################################################################

if ! test ${INSIDE_EMACS}; then
    setopt promptsubst
    autoload -U promptinit
    promptinit
    prompt wunjo
    #. ${HOME}/.zshprompt
fi

local be_quite
[ $TERM = "dumb" ] && unsetopt zle && PS1='$ ' && unsetopt prompt_cr && be_quite=yeah

################################################################################
# and finally... ($MACHINE is set in ~/.zshenv)
echo "Welcome to $MACHINE, $USER. Local time is `date +%c`"
echo
#################################### FIN #######################################

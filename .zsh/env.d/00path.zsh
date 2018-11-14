PATH="/usr/local/bin:/usr/local/sbin:${PATH}"
[ -d "${HOME}/local/bin" ] && PATH="${HOME}/local/bin:${PATH}"
[ -d "${HOME}/.bin" ] && PATH="${HOME}/.bin:${PATH}"
[ -d "${HOME}/.cargo/bin" ] && PATH="${HOME}/.cargo/bin:${PATH}"
[ -d "${HOME}/Code/go/bin" ] && PATH="${HOME}/Code/go/bin:${PATH}"
[ -d "/opt/local/bin" ] && PATH="${PATH}:/opt/local/bin"

export PATH

MANPATH="/opt/local/share/man:$MANPATH"
export MANPATH

typeset -U path cdpath fpath

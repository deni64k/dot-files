export PATH=/usr/local/bin:/usr/local/sbin:${PATH}
[ -d "${HOME}/.cargo/bin" ] && PATH="${HOME}/.cargo/bin:${PATH}"
[ -d "${HOME}/Code/go/bin" ] && PATH="${HOME}/Code/go/bin:${PATH}"
[ -d "${HOME}/.bin" ] && PATH="${HOME}/.bin:${PATH}"

export PATH

typeset -U path cdpath fpath

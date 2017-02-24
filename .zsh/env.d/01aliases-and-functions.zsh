alias df='df -h'

alias ltr="ls -ltr"

alias mg='mg -n'

alias gi2p='GIT_PROXY_COMMAND="${HOME}/.bin/i2p-socks-proxy" GIT_SSH="${HOME}/.bin/i2p-socks-ssh" git'

strerror() {
  perl -MPOSIX -e "print strerror($1).\"\\n\";"
}

if ! type pbpaste >/dev/null 2>&1; then
  alias pbpaste="xclip -o"
fi

if ! type pbcopy >/dev/null 2>&1; then
  alias pbcopy="xclip -sel clip"
fi

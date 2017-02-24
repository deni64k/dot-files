alias df='df -h'

alias ltr="ls -ltr"

alias e="$EDITOR"
alias E="$EDITOR"

alias gi2p='GIT_PROXY_COMMAND="${HOME}/.bin/i2p-socks-proxy" GIT_SSH="${HOME}/.bin/i2p-socks-ssh" git'

strerror() {
  perl -MPOSIX -e "print strerror($1).\"\\n\";"
}

if ! type pbpaste; then
  alias pbpaste="xclip -o"
  alias pbcopy="xclip -sel clip"
fi

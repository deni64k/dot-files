PATH="/bin:/sbin:/usr/bin:/usr/sbin:\
/usr/local/bin:/usr/local/sbin:/usr/games"
if test "x$USER" != 'xroot'; then
    PATH="${HOME}/bin:${HOME}/.cabal/bin:${PATH}"
fi
export PATH

# df
export BLOCKSIZE=K

machine="`hostname`"
# set up placement per machine
case ${machine} in
    dennis.home|puffy.home)
        placement="home"
        ;;

    dsuhonin|fedorka|sorm)
        placement="work"
        ;;
esac

export MACHINE=${machine}
export PLACEMENT=${placement}

[ -f ${HOME}/.zshenv.local ] && . ${HOME}/.zshenv.local

# More nice colors for ls, don't?
export CLICOLOR="YES" LSCOLORS="ExGxFxdxCxDxDxhbadExEx"
# Cyrillic for darcs
export DARCS_DONT_ESCAPE_EXTRA="йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮ"

which-maybe ()
    which -s $1 >/dev/null 2>&1;

if   which-maybe emacsclient; then
    EDITOR='emacsclient -t'
    VISUAL='emacsclient -t'
elif which-maybe mg; then
    EDITOR='mg'
    VISUAL='mg'
fi
export EDITOR VISUAL

if   which-maybe most; then
    PAGER='most'
elif which-maybe less; then
    export LESS='-RM'
    PAGER='less'
elif which-maybe more; then
    PAGER='more'
fi
export PAGER

export RI='--format ansi'

# http://ru.download.nvidia.com/freebsd/173.14.12/README/chapter-08.html
__GL_FSAA_MODE=7      # 4x Bilinear Multisampling by 4x Supersampling
__GL_LOG_MAX_ANISO=3  # 8x anisotropic filtering
__GL_SYNC_TO_VBLANK=1

# for send-pr
if which-maybe send-pr; then
    export GNATS_ADDR='FreeBSD-gnats-submit@freebsd.org'
    export MAIL_AGENT="${MAIL_AGENT:-msmtp ${GNATS_ADDR}}"
fi

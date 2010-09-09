[[ -f /etc/profile ]] && source /etc/profile

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

# BSD ls
export CLICOLOR="YES" LSCOLORS="ExGxFxdxCxDxDxhbadExEx"
# GNU ls
export LS_COLORS='no=00;37:fi=00;37:di=01;36:ln=04;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:tw=30;42:ow=34;42:st=37;44:ex=00:*.cmd=01;31:*.exe=01;31:*.com=01;31:*.btm=01;31:*.bat=01;31:*.sh=01;31:*.zsh=01;31:*.run=01;31:*.tar=33:*.tgz=33:*.arj=33:*.taz=33:*.lzh=33:*.zip=33:*.z=33:*.Z=33:*.gz=33:*.bz2=33:*.7z=33:*.deb=33:*.rpm=33:*.jar=33:*.rar=33:*.jpg=32:*.jpeg=32:*.gif=32:*.bmp=32:*.pbm=32:*.pgm=32:*.ppm=32:*.tga=32:*.xbm=32:*.xpm=32:*.tif=32:*.tiff=32:*.png=32:*.mov=34:*.mpg=34:*.mpeg=34:*.avi=34:*.mkv=34:*.fli=34:*.flv=34:*.3gp=34:*.mp4=34:*.divx=34:*.gl=32:*.dl=32:*.xcf=32:*.xwd=32:*.flac=35:*.mp3=35:*.mpc=35:*.ogg=35:*.wav=35:*.m3u=35:'
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

# Ruby Version Manager (RVM)
[[ -s $HOME/.rvm/scripts/rvm ]] && source $HOME/.rvm/scripts/rvm

export ZSH_HIGHLIGHT_HIGHLIGHTERS_DIR=/usr/local/share/zsh-syntax-highlighting/highlighters
source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)

ZSH_HIGHLIGHT_STYLES[precommand]=fg=green,bold
ZSH_HIGHLIGHT_STYLES[path]=bold
ZSH_HIGHLIGHT_STYLES[path_prefix]=bold
ZSH_HIGHLIGHT_STYLES[path_approx]=fg=yellow,bold

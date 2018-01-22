if test -f "${ZSH_HIGHLIGHT_HIGHLIGHTERS_DIR}"; then
  source "${ZSH_HIGHLIGHT_HIGHLIGHTERS_DIR}"

  ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)

  ZSH_HIGHLIGHT_STYLES[precommand]=fg=green,bold
  ZSH_HIGHLIGHT_STYLES[path]=bold
  ZSH_HIGHLIGHT_STYLES[path_prefix]=bold
  ZSH_HIGHLIGHT_STYLES[path_approx]=fg=yellow,bold
fi

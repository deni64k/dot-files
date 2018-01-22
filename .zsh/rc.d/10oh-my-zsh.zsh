set -A plugins
plugins+=(github web-search                                    \
         gpg-agent rsync                                      \
         python pyenv pep8 pylint pip virtualenv fabric       \
         go                                                   \
         redis-cli postgres                                   \
         git git-extras gitignore mercurial svn               \
         docker docker-compose docker-machine                 \
         jira                                                 \
         colored-man-pages zsh-navigation-tools zsh_reload    \
        )

hash dnf >/dev/null 2>&1       && plugins+=(dnf)
hash osascript >/dev/null 2>&1 && plugins+=(osx brew)
hash dpkg >/dev/null 2>&1      && plugins+=(debian)

hash xcodebuild >/dev/null 2>&1 && plugins+=(xcode)
hash nmap >/dev/null 2>&1       && plugins+=(nmap)

if [ -d "${ZSH}" ]; then
  source $ZSH/oh-my-zsh.sh
fi

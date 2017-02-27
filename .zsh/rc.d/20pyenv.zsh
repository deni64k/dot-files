if command -v pyenv > /dev/null; then
  eval "$(pyenv init -)"

  export PYENV_ROOT=${HOME}/.pyenv

  mkdir -p ${PYENV_ROOT}
fi

if hash pyenv >/dev/null 2>&1; then
  eval "$(pyenv init -)"

  export PYENV_ROOT=${HOME}/.pyenv
  mkdir -p ${PYENV_ROOT}
fi

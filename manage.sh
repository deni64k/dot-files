#!/usr/bin/env bash

install () {
    return 0
}

pull () {
    return 0
}

status () {
    find . --exclude ./manage.sh --print --exec diff {} ~/{} \;
}

case $1 in
    install)
        install
        ;;

    pull)
        pull
        ;;

    status)
        status
        ;;

    git)
        shift
        curdir=$(pwd -P)
        (cd ~
         GIT_DIR=$curdir/.git command git "$@"
        )
        ;;
esac

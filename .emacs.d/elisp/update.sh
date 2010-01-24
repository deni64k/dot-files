#!/bin/sh

# update cvs projects
for dir in cedet ecb slime w3m; do
    (cd $dir; cvs -q up -Pd)
done

# update git projects
for dir in jabber magit undo-tree; do
    (cd $dir; git pull)
done

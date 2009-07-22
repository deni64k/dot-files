#!/usr/bin/env zsh

# update cvs projects
for dir in cedet ecb slime w3m; do
    (cd $dir; cvs -q up -Pd)
done

# update git projects
for dir in jabber magit; do
    (cd $dir; git pull)
done

# update org
# rm -rf org
# rm -f org-snapshot.tar.gz*
# wget http://orgmode.org/org-snapshot.tar.gz
# tar zxf org-snapshot.tar.gz
# mv org-snapshot org

# cedet
(cd cedet && gmake clean-all all)

# ecb
(cd ecb && gmake CEDET="../cedet")

# w3m
(cd w3m && ./configure && gmake)

# jabber
(cd jabber && ./configure && gmake)

# magit
(cd magit && ./configure && gmake)

# org
# (cd org && gmake)

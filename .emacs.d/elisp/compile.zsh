#!/bin/sh

if which gmake >/dev/null 2>&1; then
    GMAKE=gmake
elif which make >/dev/null 2>&1; then
    GMAKE=make
else
    echo "Could not found GNU Make."
    exit 1
fi

# cedet
(cd cedet && ${GMAKE} clean-all all)

# ecb
(cd ecb && ${GMAKE} CEDET="../cedet" clean ecb)

# w3m
(cd w3m && ./configure && ${GMAKE} clean all)

# jabber
(cd jabber && ./configure && ${GMAKE} clean all)

# magit
(cd magit && ./configure && ${GMAKE} clean all)

# emms
(cd emms && ${GMAKE} clean all)

# org
# (cd org && gmake)

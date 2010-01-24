#!/bin/sh

if which gmake >/dev/null 2>&1; then
    GMAKE=gmake
else
    GMAKE=make
fi

# cedet
(cd cedet && ${GMAKE} clean-all all)

# ecb
(cd ecb && ${GMAKE} CEDET="../cedet" clean ecb)

# w3m
(cd w3m && ./configure && ${GMAKE} clean all)

# jabber
(cd jabber && autoreconf -i && ./configure && ${GMAKE} clean all)

# magit
(cd magit && ./configure && ${GMAKE} clean all)

# emms
(cd emms && ${GMAKE} clean all)

#!/bin/sh

set +e

(cd cedet
 make
)

(cd magit
 ./autogen.sh
 ./configure
 make
)

(cd jabber
 autoreconf -i
 ./configure
 make
)

(cd distel
 make
)

(cd w3m
 autoconf
 ./configure
 make
)

(cd emms
 make
)

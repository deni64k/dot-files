#!/bin/sh

if ! type brew >/dev/null 2>&1; do
  /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

brew upgrade

brew tap homebrew/cask-versions
brew tap railwaycat/emacsmacport

brew install mg htop git cmake ninja gcc

packages=                                       \
        adns                                    \
	ffmpeg                                  \
	imagemagick                             \
        pyenv                                   \
        gnu-sed                                 \
	mercurial                               \
	sphinx-doc                              \
        gnu-time                                \
	opencv                                  \
	gnupg                                   \
	mg                                      \
	q                                       \
	fmt                                     \
	gnuplot                                 \
	jemalloc                                \
        mitmproxy                               \
	fontconfig                              \
	jo                                      \
        openssl                                 \
	go                                      \
	mosh                                    \
	szip                                    \
        avrdude                                 \
	google-benchmark                        \
	jq                                      \
        most                                    \
	tbb                                     \
        awscli                                  \
	graphicsmagick                          \
	bazel                                   \
	mplayer                                 \
	boost                                   \
	gawk                                    \
	graphviz                                \
	rbenv                                   \
	gcc                                     \
	grc                                     \
	multimarkdown                           \
	pcre                                    \
	readline                                \
	grep                                    \
	pcre2                                   \
	gdb                                     \
	nasm                                    \
	perl-build                              \
	harfbuzz                                \
	unbound                                 \
        chruby                                  \
	highlight                               \
	ruby-build                              \
	webp                                    \
        clang-format                            \
	pkg-config                              \
	wget                                    \
        htop                                    \
	ninja                                   \
	plenv                                   \
	wxmac                                   \
        cmake                                   \
	nmap                                    \
	plotutils                               \
	x264                                    \
        coreutils                               \
	git                                     \
	icu4c                                   \
	node                                    \
	x265                                    \
        git-extras                              \
	git-open                                \
	numpy                                   \
	protobuf                                \
	xz                                      \
        djvulibre                               \
	doxygen                                 \
	eigen                                   \
	octave                                  \
	sdl                                     \
	zeromq                                  \
        emacs-mac                               \
	llvm                                    \
	pstree                                  \
	sdl2                                    \
	zsh                                     \
        emscripten                              \
	zsh-syntax-highlighting                 \
        lua                                     \
	pv
brew install $packages

brew install --with-modules --with-xml2 --with-spacemacs-icon --with-imagemagick emacs-mac
ln -sf /usr/local/opt/emacs-mac/Emacs.app /Applications

packages=                                       \
        1password                               \
        adobe-acrobat-reader                    \
        airmail-beta                            \
        cheatsheet                              \
        cleanmymac                              \
        docker                                  \
        gimp                                    \
        google-chrome                           \
        grandperspective                        \
        inkscape                                \
        mactex                                  \
        ngrok                                   \
        pdfsam-basic                            \
        slack                                   \
        turbovnc-viewer                         \
        virtualbox                              \
        virtualbox-extension-pack               \
        vnc-viewer                              \
        wireshark                               \
        wireshark-chmodbpf                      \
        x-moto                                  \
        xquartz
brew cask install $packages

brew cleanup

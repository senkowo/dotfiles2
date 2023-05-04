#!/data/data/com.termux/files/usr/bin/bash
pkg upgrade

# install common stuff, plus desktop utils
pkg install vim emacs git neofetch man openssh cmake libtool \
    x11-repo termux-api tigervnc

pip install yt-dlp

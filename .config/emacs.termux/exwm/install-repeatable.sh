#!/data/data/com.termux/files/usr/bin/bash
pkg upgrade

# install common stuff, plus desktop utils
pkg install vim emacs git neofetch man p7zip openssh cmake libtool \
    x11-repo termux-api tigervnc xorg-server

pip install yt-dlp

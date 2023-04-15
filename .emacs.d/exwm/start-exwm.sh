#!/bin/sh
# Set screen DPI (for high DPI displays) (before compton)
#xrdb ~/.emacs.d/exwm/Xresources

# Screen compositor
picom &

# Enable screen locking on suspend
xss-lock -- slock &

# Start Emacs w/ EXWM
exec dbus-launch --sh-syntax --exit-with-session emacs -q --load "~/.emacs.d/init.el" -mm --debug-init -l ~/.emacs.d/desktop.el

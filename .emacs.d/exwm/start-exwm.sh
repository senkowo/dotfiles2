#!/bin/sh
# Set screen DPI (for high DPI displays) (before compton)
#xrdb ~/.emacs.d/exwm/Xresources

# set up dvorak and nocaps (symbols: compose:sclk)
# how to disable caps on login?
setxkbmap -layout 'us,us' -variant 'dvorak,' -option grp:alts_toggle ctrl:nocaps

# bind S-<enter> to escape
# Breaks C-S-c !
#xmodmap ~/.dotfiles/.emacs*/exwm/Xmodmap
xmodmap -e "keysym Menu = Super_R"

# set hold type startup and speed (delay rate)
xset r rate 240 60

# set cursor type (what if not enabled?)
xsetroot -cursor_name left_ptr

# Screen compositor
picom &

# Enable screen locking on suspend
xss-lock -- slock &

# Start Emacs w/ EXWM
exec dbus-launch --sh-syntax --exit-with-session emacs -q --load "~/.emacs.d/init.el" -mm --debug-init -l ~/.emacs.d/desktop.el
# exec dbus-launch --sh-syntax --exit-with-session emacs -q --load "~/.emacs.d/init.el" -mm --debug-init --start-exwm

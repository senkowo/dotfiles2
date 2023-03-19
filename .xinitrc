#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/Xresources
sysmodmap=/etc/X11/xinit/Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

## Security

defaultserverargs="-nolisten tcp"

## Start system utils 

gentoo-pipewire-launcher & # Disabled for EXWM, also has to start after booting up wm...?
xsetroot -cursor_name left_ptr & # cursor
#setxkbmap -option caps:escape
#setxkbmap -layout "us,us" -variant ",dvp" -option grp:alts_toggle
#setxkbmap -layout "us,us" -variant ",dvp" -option grp:alts_toggle ctrl:nocaps
setxkbmap -layout "us,us" -variant ",dvorak" -option grp:alts_toggle ctrl:nocaps
xmodmap ~/.Xmodmap
xset r rate 300 40
#sh ~/.local/bin/sct-auto-adjust.sh & # redshift

## Start userspace utils

xrdb ~/.Xresources &
exec urxvtd --quiet --opendisplay --fork & # term

#exec emacs --daemon &

~/.fehbg & # wallpaper

#picom & # maybe not on dwm? # picom


## Other basic things

# Keyboard
#setxkbmap

# Display
#xrandr --output Virtual-1 --mode 1920x1080 &

##-------------------
## xmonad only?
#exec trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent false --tint 0x5f5f5f --height 10 &
#exec xscreensaver -no-splash &
#if [ -x /usr/bin/nm-applet ] ; then
#	exec nm-applet --sm-disable &
#fi
##-------------------

# dwm only?
#exec ~/.local/bin/xsetloop.sh &
#exec xsetroot -name "Battery: $(cat /sys/class/power_supply/BAT0/capacity)" &


#exec dbus-launch --sh-syntax --exit-with-session xmonad >& /tmp/.xmonad-errors
#exec dbus-launch --sh-syntax --exit-with-session cwm
#exec dbus-launch --sh-syntax --exit-with-session herbstluftwm --locked
#exec dbus-launch --sh-syntax --exit-with-session dwm >/dev/null 2>&1
#exec dbus-launch --sh-syntax --exit-with-session qtile start
#exec dbus-launch --sh-syntax --exit-with-session bspwm
#exec ~/.local/bin/dwm-start-script.sh
#exec dbus-launch --sh-syntax --exit-with-session leftwm 
#exec dbus-launch --sh-syntax --exit-with-session xmonad >& /tmp/.xmonad-errors
#emacs --daemon -f exwm-enable && exec dbus-launch --sh-syntax --exit-with-session emacsclient -c
#exec i3
exec dbus-launch --sh-syntax --exit-with-session emacs
#exec dbus-launch --sh-syntax --exit-with-session $HOME/.cache/xmonad/xmonad-x86_64-linux >& /tmp/.xmonad-errors





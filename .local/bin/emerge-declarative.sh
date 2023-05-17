#!/bin/bash

## A declarative Portage package management setup ###

APPLICATIONS="
games-util/steam-launcher  ### Gaming ###
games-util/steam-meta
games-util/gamemode
media-libs/mesa
media-libs/vulkan-loader
games-emulation/dolphin
media-gfx/gimp  ### Media ###
media-gfx/mypaint
media-gfx/nomacs
media-gfx/sxiv
www-client/librewolf  ### Web ###
mail-client/thunderbird-bin
net-im/discord
app-emulation/qemu 
app-emulation/virt-manager
app-admin/keepassxc
app-editors/emacs
kde-misc/kdeconnect
sys-apps/flatpak
media-video/mpv
www-client/w3m

"

DESKTOP="
x11-base/xorg-server
x11-misc/polybar
x11-misc/picom
x11-misc/sct
app-misc/brightnessctl
x11-misc/slock
x11-misc/xss-lock
x11-apps/setxkbmap
media-gfx/feh
media-gfx/flameshot
media-gfx/scrot
sys-power/acpi
media-sound/mpd
net-p2p/syncthing
media-video/pipewire
sys-apps/xdg-desktop-portal
x11-misc/trayer
sys-apps/firejail
x11-apps/xrandr
x11-misc/arandr
x11-apps/xinput
x11-apps/xsetroot
x11-apps/xmodmap

"

FONTS="
media-fonts/cantarell
media-fonts/dejavu
media-fonts/fira-code
media-fonts/fontawesome
media-fonts/hack
media-fonts/ja-ipafonts
media-fonts/jetbrains-mono
media-fonts/liberation-fonts
media-fonts/roboto
media-fonts/terminus-font

"

TERMINAL="
x11-terms/rxvt-unicode
x11-misc/urxvt-font-size
x11-terms/alacritty
app-editors/nano
app-editors/vim
app-misc/cmatrix
app-admin/stow
app-misc/neofetch
app-misc/pfetch
app-misc/queercat
app-misc/ranger
app-misc/uwufetch
app-shells/fzf
app-shells/zsh
app-shells/zsh-syntax-highlighting
app-text/tree
sys-process/htop
sys-apps/bat
sys-apps/exa
sys-apps/ripgrep
sys-apps/fd

"

SYSTEM="
app-laptop/laptop-mode-tools
app-admin/sysklogd
app-eselect/eselect-repository
app-portage/cpuid2cpuflags
app-portage/gentoolkit
x11-drivers/xf86-input-evdev
x11-drivers/xf86-video-intel
sys-power/thermald
dev-util/pkgdev
sys-kernel/dracut
sys-kernel/genkernel
sys-kernel/gentoo-kernel-bin
sys-kernel/gentoo-sources
sys-kernel/linux-firmware
sys-process/cronie
net-firewall/ufw
net-misc/chrony
net-misc/dhcpcd
net-wireless/iwd
net-wireless/wpa_supplicant
sys-boot/grub
sys-fs/dosfstools

"

UTILS="
app-admin/doas
app-misc/trash-cli
sys-power/powertop
sys-power/suspend
dev-vcs/git
dev-vcs/lazygit
dev-util/rust-analyzer
net-misc/yt-dlp

"

ANALYSIS="
dev-util/strace
net-analyzer/ifstat
net-analyzer/nethogs
sys-apps/hwinfo
sys-apps/i2c-tools
sys-apps/mlocate
sys-apps/nvme-cli
sys-apps/pciutils
sys-process/iotop
sys-apps/smartmontools
sys-apps/usbutils
x11-apps/igt-gpu-tools

"

MISC="
dev-lang/rust-bin
dev-python/pip

"

MAYBE="
net-wireless/iw
virtual/jdk
sys-apps/system76-driver
x11-misc/xdotool
x11-misc/wmctrl
x11-misc/xclip

"

########################
##                    ##
## List of Variables: ##
##   - APPLICATIONS   ##
##   - DESKTOP        ##
##   - FONTS          ##
##   - TERMINAL       ##
##   - SYSTEM         ##
##   - UTILS          ##
##   - ANALYSIS       ##
##   - MISC           ##
##   - MAYBE          ##
##                    ##
########################

RUNSCRIPT () {

    clear

    echo
    echo "Package types: "
    echo "- applications (a)"
    echo "- desktop (d)"
    echo "- fonts (f)"
    echo "- terminal (t)"
    echo "- system (s)"
    echo "- utils (u)"
    echo "- analysis (y)"
    echo "- misc (m)"
    echo "- maybe"
    echo
    
    echo -en "Enter package type to add to @world (e.g. desktop, d, etc): \n> "
    read in

    ## lowercase input
    in=${in,,}    
    
    case $in in
	applications | a)
	    LIST=$APPLICATIONS
	    ;;
	desktop | d)
	    LIST=$DESKTOP
	    ;;
	fonts | f)
	    LIST=$FONTS
	    ;;
	terminal | t)
	    LIST=$TERMINAL
	    ;;
	system | s)
	    LIST=$SYSTEM
	    ;;
	utils | u)
	    LIST=$utils
	    ;;
	analysis | y)
	    LIST=$ANALYSIS
	    ;;
	misc | m)
	    LIST=$MISC
	    ;;
	maybe)
	    LIST=$MAYBE
	    ;;
	*)
	    echo -e "\nInvalid input, please try again."
	    sleep 1
	    return 1
	    ;;
    esac

    ## remove package category (before "/") and print packages 
    SHOW_LIST=$(sed 's|.*/||g' <<< "$LIST")
    echo -e "\nCommand to run: \ndoas emerge -nw $SHOW_LIST\n"

    ## "pretty list" (remove \n and comments)
    PLIST=$(sed 's|#.*||g' <<< "$LIST")
    PLIST=$(echo $PLIST | tr -d '\n')

    ## emerge the packages in "pretty list"
    doas emerge -nw $PLIST

    return 0

}

## main

while true
do    
    RUNSCRIPT

    if [ $? -eq 0 ]; then
	exit  
    fi
    
done


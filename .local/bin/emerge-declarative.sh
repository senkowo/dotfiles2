#!/bin/bash

## A declarative Portage package management setup ###

APPLICATIONS="
##
##[ -- APPLICATIONS -- ]##
##
#--- Gaming ----
games-util/steam-launcher
games-util/steam-meta
games-util/gamemode
media-libs/mesa
media-libs/vulkan-loader
games-emulation/dolphin
#--- Media ----
media-gfx/gimp
media-gfx/mypaint
media-gfx/nomacs
media-gfx/sxiv
media-video/mpv
www-client/w3m
#--- Net ----
www-client/librewolf
mail-client/thunderbird-bin
net-im/discord
kde-misc/kdeconnect
#--- Env ----
app-editors/emacs
sys-apps/flatpak
app-emulation/qemu
app-emulation/virt-manager
#--- Misc ----
app-admin/keepassxc
"

DESKTOP="
##
##[ -- DESKTOP -- ]##
##
#--- Xorg ----
x11-base/xorg-server
#--- Graphical ----
x11-misc/polybar
x11-misc/trayer
app-misc/brightnessctl
x11-misc/sct
x11-misc/picom
media-gfx/feh
media-gfx/scrot
media-gfx/flameshot
x11-misc/arandr
x11-misc/dunst
#--- Functional ----
x11-misc/slock
x11-misc/xss-lock
media-sound/mpd
net-p2p/syncthing
app-admin/stow
net-misc/yt-dlp
#--- Utils ----
x11-apps/setxkbmap # keyboard
x11-apps/xmodmap
x11-apps/xsetroot
sys-power/acpi
x11-apps/xrandr
x11-apps/xinput
#--- System ----
media-video/pipewire
sys-apps/firejail
#--- Misc ----
sys-apps/xdg-desktop-portal
"

FONTS="
##
##[ -- FONTS -- ]##
##
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
##
##[ -- TERMINAL -- ]##
##
#--- Terminals ----
x11-terms/rxvt-unicode
x11-misc/urxvt-font-size
x11-terms/alacritty
#--- Editors ----
app-editors/nano
app-editors/vim
#--- Shells ----
app-shells/fzf
app-shells/zsh
app-shells/zsh-syntax-highlighting
#--- Fun ----
app-misc/cmatrix
app-misc/neofetch
app-misc/pfetch
app-misc/queercat
app-misc/uwufetch
#--- Tools ----
app-misc/ranger
app-text/tree
sys-process/htop
#--- Utils ----
sys-apps/bat
sys-apps/exa
sys-apps/ripgrep
sys-apps/fd
"

SYSTEM="
##
##[ -- SYSTEM -- ]
##
#--- Kernel ----
sys-kernel/dracut
sys-kernel/genkernel
sys-kernel/gentoo-kernel-bin
sys-kernel/gentoo-sources
sys-kernel/linux-firmware
sys-boot/grub
sys-fs/dosfstools
#--- Portage ----
app-eselect/eselect-repository
app-portage/gentoolkit
app-portage/cpuid2cpuflags
dev-util/pkgdev
#--- System ----
app-laptop/laptop-mode-tools
app-admin/sysklogd
sys-power/thermald
sys-process/cronie
x11-drivers/xf86-input-evdev # ? remove since unknown ?
x11-drivers/xf86-video-intel # ? for transparency ?
#--- Net ----
net-firewall/ufw
net-misc/chrony
net-misc/dhcpcd
net-wireless/iwd
net-wireless/wpa_supplicant
"

UTILS="
##
##[ -- UTILS -- ]##
##
#--- System ----
app-admin/doas
app-misc/trash-cli
sys-power/suspend
#--- Tools ----
dev-vcs/git
dev-vcs/lazygit
dev-util/rust-analyzer
"

ANALYSIS="
##
##[ -- ANALYSIS -- ]##
##
#--- Net ----
net-analyzer/nethogs  ## identify process using bandwidth
net-analyzer/ifstat  ## network device upload/download
#--- SSD ----
sys-apps/nvme-cli  ## userspace tooling (?)
sys-process/iotop  ## process IO
sys-apps/smartmontools  ## smartctl, ssd lifespan
#--- Power ----
sys-power/powertop  ## power consumption and cpu wakeups
#--- Other ----
sys-apps/hwinfo  ## describe all hardware devices
sys-apps/i2c-tools  ## i2c tools like scan i2c bus for devices (?)
sys-apps/pciutils  ## provides lspci (detects hardware connected to the pci bus)
sys-apps/usbutils  ## provides lsusb
x11-apps/igt-gpu-tools  ## userspace gpu tools (?)
#--- Misc ----
dev-util/strace  ## view command syscalls (useful for firejail troubleshooting)
"

MISC="
##
##[ -- MISC -- ]##
##
dev-lang/rust-bin
"

MAYBE="
##
##[ -- MAYBE -- ]##
##
net-wireless/iw
virtual/jdk
sys-apps/system76-driver
x11-misc/xdotool
x11-misc/wmctrl
x11-misc/xclip
sys-apps/mlocate
dev-python/pip  ## only using for hyfetch? (req. --user on run) (use guix instead??)
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
    echo "- all"
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
	    LIST=$UTILS
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
	all) # excludes $MAYBE
	    LIST="$APPLICATIONS $DESKTOP $FONTS $TERMINAL $SYSTEM $UTILS $ANALYSIS $MISC"
	    ;;
	*)
	    echo -e "\nInvalid input, please try again."
	    sleep 1
	    return 1
	    ;;
    esac

    ## remove package category (before "/") and print packages 
    SHOW_LIST=$(sed 's|.*/||g' <<< "$LIST")
    echo -e "\nCommand to run: \ndoas emerge -nw \n$SHOW_LIST\n"

    ## "final list" (remove \n and comments)
    FLIST=$(sed 's|#.*||g' <<< "$LIST")
    FLIST=$(echo $FLIST | tr -d '\n')

    ## TEST
    # echo $FLIST

    ## emerge the packages in "final list"
    doas emerge -nwp $FLIST

    return 0

}

## main

while true
do    
    RUNSCRIPT # returns 1 if error ; else 0

    if [ $? -eq 0 ]; then
	exit  
    fi
    
done


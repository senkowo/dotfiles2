#!/data/data/com.termux/files/usr/bin/bash
##
## A bash script that installs the desktop
##

cd ~

repeatable() {

    echo -e -n "\nRun install-repeatable.sh script? \n(updates and installs all packages) \n> "
    read in
    if [[ "$in" == "y" ]]; then
	bash ~/dotfiles2/.config/emacs.termux/exwm/install-repeatable.sh
    fi

}

setup_storage() {

    echo
    ls ~

    echo -en "\nRun termux-setup-storage? \n> "
    read in
    if [[ "$in" == "y" ]]; then
	termux-setup-storage
    fi

}

# termux_properties() {

#     echo
#     cd ~/.termux
#     ls
#     termux_prop_file="~/.termux/termux.properties"
#     prop_extra="extra-keys = [['TAB','ESC','PGUP','PGDN']]"
#     echo -e "\nLines to enter: \n\n$prop_extra\n"
#     echo -e "\nCurrent config: <Placeholder: Last few lines of file> \n<Keep symlink from termux dir to dotfiles? Consider simply keeping the file?>/n"
#     echo -en "\nAdd this line to the config? \n> "
#     read in
#     if [[ "$in" == "y" ]]; then
# 	$($termux_prop_file << $(echo "##############"))
# 	$($termux_prop_file << $prop_extra)

# 	echo "UPDATED FILE:"
# 	echo "#################\n"
# 	cat ~/.termux/termux.properties
# 	echo "##################\n"
#     fi

# }

git_clone() {

    echo
    ls ~

    echo -en "\nGit clone dotfiles? \n> "
    read in
    if [[ "$in" == "y" ]]; then
	cd ~
	git clone https://github.com/senkowo/dotfiles2
    fi

}

setup_vnc() {

    echo
    ls -a
    vncserver -list

    echo -en "\nSetup vnc server? \n> "
    read in
    if [[ "$in" == "y" ]]; then
	vncserver -localhost
    fi

}

symlinks_helper() {

    full=$1
    dest=$(echo $full | awk -v N=$4 '{print $4}')
    dir=$(echo $dest | sed 's|[^/]*$||')

    echo "Symlink to create: $full"

    if [[ -d $dir ]]; then
	echo "Directory $dir doesn't exist. Enter to create..."
	read in
	mkdir -p $dir
    fi
    if [[ -f $dest ]]; then
	echo "Regular file exists here. Enter to delete.,."
	read in
	rm $dest
    fi
    if [[ ! -L $dest ]]; then
	echo "Enter to create symlink..."
	read in
	$($full)
    else
	echo "Error how tf did this happen"
	exit 1
    fi

}

symlinks() {

    ls -a ~/dotfiles2
    echo -en "\nSymlink all possible files? \n> "
    read in
    if [[ "$in" == "y" ]]; then
	# .bashrc
	link1="ln -s ~/dotfiles2/.config/emacs.termux/exwm/.bashrc ~/.bashrc"

	# termux.properties
	link2="ln -s ~/dotfiles2/.config/emacs.termux/exwm/termux.properties ~/.termux/termux.properties"

	# startdesktop command
	link3="ln -s ~/dotfiles2/.config/emacs.termux/exwm/startdesktop ~/.local/bin/startdesktop"

	# xstartup
	link4="ln -s ~/dotfiles2/.config/emacs.termux/exwm/xstartup ~/.vnc/xstartup"

	# vnc-config
	link5="ln -s ~/dotfiles2/.config/emacs.termux/exwm/vnc-config ~/.vnc/config"

	symlinks_helper "$link1"
	symlinks_helper "$link2"
	symlinks_helper "$link3"
	symlinks_helper "$link4"
	symlinks_helper "$link5"

    fi

}

install_run() {
    repeatable
    setup_storage
    git_clone
    setup_vnc
    symlinks
}

## main
install_run

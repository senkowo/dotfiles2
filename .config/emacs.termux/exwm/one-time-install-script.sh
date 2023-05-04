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

termux_properties() {

    ## fix regex, might need awk
    ## Place after "#########" after "# Extra keys"
    ## if line blank or if new exists there

    ## also fix simlinking
    ## if file (not link), rm. then, sym regardless

    echo
    cd ~/.termux
    ls
    echo -en "\nSetup termux.properties? \n> "
    read in
    if [[ "$in" == "y" ]]; then
	#sed -i 's/# extra-keys\b/extra-keys/g' ~/.termux/termux.properties
	echo "tofix"
    fi

}

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
	$($full)
    fi

}

symlinks() {

    ls -a ~/dotfiles2
    echo -en "\nSymlink all possible files? \n> "
    read in
    if [[ "$in" == "y" ]]; then
	# .bashrc
	link1="ln -s ~/dotfiles2/.config/emacs.termux/exwm/.bashrc ~/.bashrc"

	# startdesktop command
	link2="ln -s ~/dotfiles2/.config/emacs.termux/exwm/startdesktop ~/.local/bin/startdesktop"

	# xstartup
	link3="ln -s ~/dotfiles2/.config/emacs.termux/exwm/xstartup ~/.vnc/xstartup"

	# vnc-config
	link4="ln -s ~/dotfiles2/.config/emacs.termux/exwm/vnc-config ~/.vnc/config"

	symlinks_helper "$link1"
	symlinks_helper "$link2"
	symlinks_helper "$link3"
	symlinks_helper "$link4"

    fi

}

install_run() {
    repeatable
    setup_storage
    termux_properties
    git_clone
    setup_vnc
    symlinks
}

## main
install_run

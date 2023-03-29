#
## config for .zshrc
#

# Enable colors and change prompt:
autoload -U colors && colors
PS1="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}$%b "

# History in cache directory:
HISTFILE=~/.cache/zsh/history		# note: do "mkdir -p ~/.cache/zsh/" first!
HISTSIZE=100000
SAVEHIST=100000

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.

#--------------------------------------------------------------

# vi mode
bindkey -e # emacs, not -v (vim)
# export KEYTIMEOUT=1

# # Change cursor shape for different vi modes.
# function zle-keymap-select () {
#     case $KEYMAP in
#         vicmd) echo -ne '\e[1 q';;      # block
#         viins|main) echo -ne '\e[5 q';; # beam
#     esac
# }
# zle-line-init() {
#     zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
#     echo -ne "\e[5 q"
# }

# zle -N zle-keymap-select
# zle -N zle-line-init
# echo -ne '\e[5 q' # Use beam shape cursor on startup.
# preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.


#--------------------------------------------------------------

#
## End of main
#

# Export 
export HISTORY_IGNORE="(ls|cd|pwd|exit|doas reboot|history|cd -| cd ..)"
#export EDITOR="emacsclient -t -a ''"
#export VISUAL="emacsclient -c -a emacs"
#export VISUAL="emacsclient -t -a ''"
export VISUAL="emacsclient"
export EDITOR="$VISUAL"
#export MANPAGER="sh -c 'col -bx | bat -l man -p'"


# Import aliases
[ -f "$HOME/.aliasrc" ] && source "$HOME/.aliasrc"

# syntax highlighting plugin (https://github.com/zsh-users/zsh-syntax-highlighting)
source /usr/share/zsh/site-functions/zsh-syntax-highlighting.zsh

# Disable bell
unsetopt BEEP


# vterm integration

vterm_printf() {
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

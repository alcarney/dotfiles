#
# ~/.bashrc
#
# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# -- Options
#
# autocd:       If the command matches a directory name cd into it.
# cdable_vars:  Any variable set to a path can be used as a cd 'bookmark'
# globstar:     Enable recursive ** globbing
shopt -s autocd
shopt -s cdable_vars
shopt -s globstar


# -- cd bookmarks:
export dotfiles="$HOME/.config/dotfiles"
export arlunio="$HOME/Projects/arlunio/"

# -- PATH: Places to search for executables

# Create a list of places to check and potentially add to our path
paths=(
    "$HOME/bin"
    "$HOME/go/bin"
    "$HOME/.local/bin"
    "$HOME/.poetry/bin"
    "$HOME/.npm-g/bin"

)

# Loop through each path and add any that exist to the path
for p in ${paths[@]}
do
    [ -d $p ] && export PATH="$p:$PATH"
done

# -- Other Environment Variables

# LS_COLORS: Controls which colors ls will use
#
# di=1:    Bold directories
export LS_COLORS="di=1"

# PROMPT_COMMAND: Execute a command before displaying the prompt.
export PROMPT_COMMAND='echo'

# -- Aliases

# misc: Miscellaneous aliases
#
# cp:  Prompt the user if overwriting existing files
# mv:  Prompt the user if overwriting existing files
# rm:  Prompt to confirm file removal.
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'

# ls: Enhanced
#
# -C:                        List entries by columns
# -F:                        Append character to indicate the type of each file
# -h:                        Human readable file sizes
# -X:                        Sort files by extension
# --color=auto               Colorise the output
# --group-directories-first: Directories are listed before files.
alias ls='ls -CFhX --color=auto --group-directories-first'

# git: Git shortcuts
#
# gd:  Git diff
# gst: More compact git status
alias gd='git diff'
alias gst='git status -sb ; git --no-pager diff --shortstat'

# pacman: Shortcuts to common package management tasks
#
# pacin:    Install packages
# pacinfo:  Display package info for an installed package
# paclean:  Remove any packages that are no longer installed from the cache
# paclinfo: Search repositories for info on a package
# paclook:  Search repositories for a package
# pacout:   Remove a package and any of its unique dependencies
# pacup:    Update the system
alias pacin='sudo pacman -S'
alias pacinfo='pacman -Qi'
alias paclean='sudo pacman -Sc'
alias paclinfo='pacman -Si'
alias paclook='pacman -Ss'
alias pacout='sudo pacman -Rs'
alias pacup='sudo pacman -Syu'

# -- Functions

# pacis:
#
# If given an argument this function does a grep on all installed packages.
# Otherwise all packages are simply listed
pacis () {
    if [ "$1" = "" ]; then
        pacman -Q
    else
        pacman -Q | grep "$1"
    fi
}

# -- Extras


# PS1: The format of the prompt
#
# \W:  The name of the current working directory

if [ -f "/usr/share/git/completion/git-prompt.sh" ]; then
    source "/usr/share/git/completion/git-prompt.sh"

    export GIT_PS1_SHOWDIRTYSTATE=1
    export GIT_PS1_SHOWUPSTREAM='verbose'
    export PS1='\W $(__git_ps1 "(%s)")> '
else
    export PS1='\W > '
fi

# If git is installed, source the completion script
[ -f "/usr/share/git/completion/git-completion.bash" ] && source "/usr/share/git/completion/git-completion.bash"

#
# ~/.bashrc
#
# If not running interactively, don't do anything
[[ $- != *i* ]] && return


# -- Environment Variables

# LS_COLORS: Controls which colors ls will use
#
# di=1:    Bold directories
LS_COLORS="di=1"

# PROMPT_COMMAND: Execute a command before displaying the prompt.
PROMPT_COMMAND='echo'

export LS_COLORS
export PROMPT_COMMAND


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
# pacis:    Is a package installed?
# paclean:  Remove any packages that are no longer installed from the cache
# paclinfo: Search repositories for info on a package
# paclook:  Search repositories for a package
# pacout:   Remove a package and any of its unique dependencies
# pacup:    Update the system
alias pacin='sudo pacman -S'
alias pacinfo='pacman -Qi'
alias pacis='pacman -Q'
alias paclean='sudo pacman -Sc'
alias paclinfo='pacman -Si'
alias paclook='pacman -Ss'
alias pacout='sudo pacman -Rs'
alias pacup='sudo pacman -Syu'


# -- Functions

# cd: Enhanced
#
# Very simple but useful override of the built in `cd` command. Execute cd as
# normal but then automatically `ls` the dir as we enter it.
cd () {
    builtin cd $1
    ls
}

# -- Extras


# PS1: The format of the prompt
#
# \W:  The name of the current working directory

if [ -f "/usr/share/git/completion/git-prompt.sh" ]; then
    source "/usr/share/git/completion/git-prompt.sh"

    export GIT_PS1_SHOWDIRTYSTATE=1
    export GIT_PS1_SHOWUPSTREAM='verbose'
    export PS1='\W $(__git_ps1 "(%s)") > '
else
    export PS1='\W > '
fi

# If git is installed, source the completion script
[ -f "/usr/share/git/completion/git-completion.bash" ] && source "/usr/share/git/completion/git-completion.bash"

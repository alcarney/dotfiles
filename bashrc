#
# ~/.bashrc
#
# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Source global definitions
if [ -f /etc/bashrc ]; then
    source /etc/bashrc
fi

# -- Options
#
# autocd:       If the command matches a directory name cd into it.
# cdable_vars:  Any variable set to a path can be used as a cd 'bookmark'
# globstar:     Enable recursive ** globbing
# histappend:   On exit append history to HISTFILE, don't overwrite
shopt -s autocd
shopt -s cdable_vars
shopt -s globstar
shopt -s histappend

# -- Options: History
#
# HISTCONTROL:
# - erasedups:  Delete any previous instances of the command before addding
#               it to the history
#
export HISTCONTROL="ignoreboth:erasedups"

# -- cd bookmarks:
export arlunio="$HOME/Projects/arlunio/"
export blog="$HOME/Projects/blog/"
export esbonio="$HOME/Projects/esbonio"
export dotfiles="$HOME/.config/dotfiles"

# -- PATH: Places to search for executables

# Create a list of places to check and potentially add to our path
paths=(
    "$HOME/bin"
    "$HOME/go/bin"
    "$HOME/.cargo/bin"
    "$HOME/.emacs.d/bin"
    "$HOME/.local/bin"
    "$HOME/.poetry/bin"
    "$HOME/.npm-packages/bin"

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

# SSH_AUTH_SOCK: Use gpg as an ssh agent
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

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

# PS1: The format of the prompt
#
# \W:  The name of the current working directory

if [ -f "/usr/share/git-core/contrib/completion/git-prompt.sh" ]; then
    source "/usr/share/git-core/contrib/completion/git-prompt.sh"

    export GIT_PS1_SHOWDIRTYSTATE=1
    export GIT_PS1_SHOWUPSTREAM='verbose'
    export PS1='\W $(__git_ps1 "(%s)")\n> '
else
    export PS1='\W\n> '
fi

# If git is installed, source the completion script
[ -f "/usr/share/bash-completion/completions/git" ] && source "/usr/share/bash-completion/completions/git"

# Drop into the fish shell only if
# - It's available
# - We aren't running a 'one-liner' (bash -c '...')
# - Bash itself is not being launched from within fish
#[ -f "/usr/bin/fish" ] &&\
#[ -z "$BASH_EXECUTION_STRING" ] &&\
#[ "$(ps --no-headers --pid=$PPID --format=cmd)" != "fish" ] && exec fish


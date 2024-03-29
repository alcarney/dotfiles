# vim: ft=bash

# -- path
#
# If the following paths actually exist on this machine, add them to PATH
paths=(
    "$HOME/bin"
    "$HOME/.local/bin"
)

for p in ${paths[@]}
do
    [ -d $p ] && export PATH="$p:$PATH"
done

# -- prompt
__venv_py_version()
{
    if [ -z "${VIRTUAL_ENV}" ]; then
        echo ""
    else
        echo " 🐍 v$(python --version | sed 's/Python //')"
    fi
}

__nix_dev_shell()
{
    # TODO: Can we give a meaningful name to each env?
    if [ -z "${IN_NIX_SHELL}" ]; then
        echo ""
    else
        echo " (nix shell) "
    fi
}

__is_toolbox()
{
    if [ -f /run/.containerenv ] && [ -f /run/.toolboxenv ]; then
        name=$(grep name /run/.containerenv | sed 's/.*"\(.*\)"/\1/')

    else
        name="\h"
    fi

    echo "\[\e[32m\]${name}\[\e[0m\]"
}

if [ -f "/usr/share/git-core/contrib/completion/git-prompt.sh" ]; then
    source "/usr/share/git-core/contrib/completion/git-prompt.sh"

    export GIT_PS1_SHOWDIRTYSTATE=1       # (*) unstaged changes, (+) staged changes
    export GIT_PS1_SHOWSTASHSTATE=1       # ($) stashed
    export GIT_PS1_SHOWUNTRACKEDFILES=1   # (%) untracked files
    export GIT_PS1_SHOWUPSTREAM=verbose
    export GIT_PS1_SHOWCOLORHINTS=1

    export PROMPT_COMMAND='__git_ps1 "\n\w " "$(__nix_dev_shell)$(__venv_py_version)\n$(__is_toolbox) > " "[ %s]"'
else
    export PS1="\W\n> "
fi


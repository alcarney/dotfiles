#!/bin/bash

# Do we even have tmux?
which tmux > /dev/null 2> /dev/null

if [[ "$?" != "0" ]]; then
    ${TERMINAL:-st}
else

    # Right we have tmux, let's look for an existing scratch session
    tmux ls | cut -d ':' -f 1 | grep scratch > /dev/null

    if [[ "$?" != "0" ]]; then
        # Looks like we need to create one
        ${TERMINAL:-st} -e tmux new-session \; rename-session scratch &
    else
        # Let's use the one we made earlier
        ${TERMINAL:-st} -e tmux attach -t scratch &
    fi

fi

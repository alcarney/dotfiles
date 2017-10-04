# Get relevant colours so our menu matches the rest of the desktop
bgcol=$(herbstclient get frame_border_normal_color)
fgcol=$(herbstclient get_attr theme.normal.color)
bgsel=$(herbstclient get_attr theme.active.color)
fgsel=$(herbstclient get frame_border_active_color)

# Configure the dmenu command
dmenucmd=(dmenu -p "tmux" -nb "$bgcol" -nf "$fgcol" -sb "$bgsel" -sf "$fgsel")

# Get a list of available sessions
sessions=$(tmux ls | cut -d ':' -f 1)

# Pass the sessions to dmenu to get the selection
selected=$(echo -e "$sessions" | ${dmenucmd[@]})

# Was the selected item an existing session?
echo -e "$sessions" | grep "$selected" > /dev/null 2> /dev/null

if [[ "$?" == "0" ]] ; then

    # Spawn a new terminal and attach to the session
    ${TERMINAL:-st} -e tmux attach -t $selected &
    exit
fi

# If not - is an existing tmuxinator project?
projects=$(tmuxinator list | grep -v projects | awk 'BEGIN{RS="[ ]+";}{printf "%s\n", $1}')
echo -e "${projects[@]}" | grep $selected > /dev/null 2> /dev/null

if [[ "$?" == "0" ]]; then

    # Spawn a new terminal and start the project
    ${TERMINAL:-st} -e tmuxinator start $selected &
    exit
fi

# If we got this far - assume I want to start a new session with this name
${TERMINAL:-st} -e tmux new-session \; rename-session $selected &

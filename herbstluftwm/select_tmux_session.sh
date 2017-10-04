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

# Spawn a new terminal and attach to the session
${TERMINAL:-st} -e tmux attach -t $selected &

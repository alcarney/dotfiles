
# Get relevant colours so our menu matches the rest of the desktop
bgcol=$(herbstclient get frame_border_normal_color)
fgcol=$(herbstclient get_attr theme.normal.color)
bgsel=$(herbstclient get_attr theme.active.color)
fgsel=$(herbstclient get frame_border_active_color)

SSHDIR=$HOME/.ssh

# Configure the dmenu command
dmenucmd=(dmenu -p "ssh" -nb "$bgcol" -nf "$fgcol" -sb "$bgsel" -sf "$fgsel")

# Get a list of available sessions
remotes=$(grep ^host ${SSHDIR}/config | cut -d ' ' -f 2)

# Pass the sessions to dmenu to get the selection
selected=$(echo -e "$remotes" | ${dmenucmd[@]})

# Spawn a new terminal and attach to the session
${TERMINAL:-st} -e ssh $selected &

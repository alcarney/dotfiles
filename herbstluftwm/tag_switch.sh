# Get relevant colours so our menu matches the rest of the desktop
bgcol=$(herbstclient get frame_border_normal_color)
fgcol=$(herbstclient get_attr theme.normal.color)
bgsel=$(herbstclient get_attr theme.active.color)
fgsel=$(herbstclient get frame_border_active_color)

# Configure the dmenu command
dmenucmd=(dmenu -p "tag" -nb "$bgcol" -nf "$fgcol" -sb "$bgsel" -sf "$fgsel")

# Get a list of the available tags
tags=$(herbstclient tag_status | sed 's/[.:+#\-%!]//g' |\
        awk 'BEGIN{RS="\t"} {printf "%s\n", $0}')

# Pass the tags to dmenu to get the selection
selected=$(echo -e "$tags" | ${dmenucmd[@]})

# Switch to that tag
herbstclient use $selected


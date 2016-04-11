#!/bin/bash

ws () {
    wksp=$(i3-msg -t get_outputs | sed 's/.*"current_workspace":"\([^"]*\)".*/\1/')
    echo "%{F#ff0000}$wksp%{F-}"
}

lsep () {
    echo " %{F#aaaaaa}>%{F-} "
}

bg-change () {
    cmd='feh --randomize --recursive --bg-scale "/home/alex/Media/Wallpapers/"'
    echo " %{A:$cmd:} WP %{A} "
}

i3status | while :
do
    read line
    echo " $(ws) %{r} $line $(bg-change) " || exit 1
done

#!/bin/bash

ws () {
    wksp=$(i3-msg -t get_outputs | sed 's/.*"current_workspace":"\([^"]*\)".*/\1/')
    echo "%{F#ff0000}$wksp%{F-}"
}

lsep () {
    echo " %{F#aaaaaa}>%{F-} "
}

datenow () {
    datefmt="%d/%m"
    echo "%{F#ff0000}$(date +$datefmt)%{F-}"
}

timenow () {
    datefmt="%H:%M:%S"
    echo "%{F#ff0000}$(date +$datefmt)%{F-}"
}

bg-change () {
    cmd='feh --randomize --recursive --bg-scale "/home/alex/Media/Wallpapers/"'
    echo " %{A:$cmd:} WP %{A} "
}

pow () {
    cmd='/home/alex/bin/i3exit suspend'
    echo " %{A:$cmd:} L %{A} "
}

i3status | while :
do
    read line
    echo " $(ws) %{c} $(datenow) $(timenow) %{r} $line $(bg-change) $(pow)" || exit 1
done

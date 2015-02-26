#!/bin/bash

dir=$(echo ${BASH_SOURCE[0]} | sed 's/dzen_widgets.sh//')
widget_dir="$dir/dzen_widgets"

calendar ()
{
    date=$(date +"%d/%m/%y")
    timeis=$(date +"%H:%M:%S")
    echo -n "$timeis | ^ca(1, . $widget_dir/calendar.sh)^fg(red)$date ^fg()^ca()"
    return
}

status_line ()
{
    calendar
    echo
    return
}

while true
do
    echo "$(status_line)"
    sleep 1
done

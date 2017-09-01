# Source all the config variables
. ~/.config/herbstluftwm/panel_config.sh
width=250

up () {
    TIME=$(uptime -p | sed 's/up//' | sed 's/ weeks\?,/w/' | sed 's/ days\?,/d/' \
                     | sed 's/ hours\?,/h/' | sed 's/ minutes\?/m/')
    echo -n "$TIME"
}

mem () {
    MEM=$(free -h | sed -n '2p' | awk '{printf "%s / %s", $3, $2}')
    echo -n "$MEM"
}

cpu () {
    load=($(top -b -n 1 | sed -n '3,6p' | sed 's/\[|* *\]//g' | awk '{printf "%s% ", $4}'))
    n=0

    for l in ${load[@]} ; do
        echo $(padline " CPU${n}:" $l)
        n=$(($n + 1))
    done
}

{
    echo "System Information"
    while true ; do
        echo "$(padline 'Uptime:' $(up))"
        echo "$(padline 'RAM:' $(mem))"
        cpu
        sleep 1
    done
} | dzen2 -l 6 -y $panel_height -x 1550 -w $width -h $panel_height \
          -fn "$font" -bg $bgcolor -sa l \

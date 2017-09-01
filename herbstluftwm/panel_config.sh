panel_height=16
font="-*-fixed-medium-*-*-*-12-*-*-*-*-*-*-*"
bgcolor=$(herbstclient get frame_border_normal_color)

padline () {
    left=$1
    shift
    right=$@

    left_text=$(echo -n "$left" | sed 's.\^[^(]*([^)]*)..g')
    right_text=$(echo -n "$right" | sed 's.\^[^(]*([^)]*)..g')

    right_length=$(textwidth "$font" $right_text)
    left_length=$(textwidth "$font" $left_text)

    pad=$(($width - $right_length - $left_length - 27))

    echo -n " $left^pa($pad)$right"

}

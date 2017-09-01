# This is my dzen2 panel for use with herbstluftwm, it is a heavily
# based on the default panel that came with v0.7 but has been
# simplified by removing a lot of code that deals with the
# differences between platforms.
#
# It is also commented in an attempt to explain to myself how this
# all works.

# By default we assume that the monitor is "named" 0 otherwise we
# take the first argument to be the monitor name
monitor=${1:-0}

# We need to know the dimensions of the monitor to get the width
# surrounding it in parens () dims gets parsed as an array
# dims = X Y W H
dims=( $(herbstclient monitor_rect $monitor) )
panel_width=${dims[2]}
panel_height=16

# Pad the montior to make room for the panel
herbstclient pad $monitor $panel_height

# We also get the position of the top-left of the monitor too!
x=${dims[0]}
y=${dims[1]}

# Configuration for the output
bgcolor=$(herbstclient get frame_border_normal_color)
selbg=$(herbstclient get_attr theme.active.color)
selfg="#101010"
highcolor="#eeeeee"
font="-*-fixed-medium-*-*-*-12-*-*-*-*-*-*-*"

# Battery config, where is the script to get the info, and
# the function to format it
BAT=~/.config/herbstluftwm/battery.sh
batfmt () {

    if (($1 < 20)) ; then
        echo -n "^fg(#ff0000)${1}%^fg() "
    else
        echo -n "${1}% "
    fi

    if (($2 < 20)) ; then
        echo -n "^fg(#ff0000)${2}%^fg() "
    else
        echo -n "${2}% "
    fi

    case $3 in
        "C")
            echo -n "^fg(#00ff00)C^fg() ";;
        *)
            echo -n ""
    esac
}

# Tag formatting
tagfmt () {
    # Look at the first character - this tells us the state
    # of the tag, from the manpage (as of v0.7)
    # .  the tag is empty
    # : the tag is not empty
    # + the tag is viewed on the specified MONITOR, but this monitor is not focused.
    # # the tag is viewed on the specified MONITOR and it is focused.
    # - the tag is viewed on a different MONITOR, but this monitor is not focused.
    # % the tag is viewed on a different MONITOR and it is focused.
    # !  the tag contains an urgent window
    case ${1:0:1} in
        ':') echo -n "^bg()^fg(#ffffff)";;
        '#') echo -n "^bg($selbg)^fg($selfg)";;
        '!') echo -n "^bg(#ff0000)^fg(#222222)";;
        *) echo -n "^bg()^fg(#ababab)"
    esac

    # Now that the formatting is set, we now write the actual tag name
    # here we also make use of dzen's clickable area so that clicking
    # on the tag name will switch to it
    echo -n "^ca(1, herbstclient focus_monitor ${monitor} &&"
    echo -n "herbstclient use ${1:1})"
    echo -n " ${1:1} ^fg()^bg()^ca()"
}

uniq_linebuffered() {
  awk '$0 != l { print ; l=$0 ; fflush(); }' "$@"
}

net_test () {
    ping -q -w1 -c1 www.google.com > /dev/null 2> /dev/null
}

SYSINFO=~/.config/herbstluftwm/system.sh
cpuload() {
    echo -e "cpu\t$(top -b -n 1 | head -n 1 | cut -d ',' -f 4 | cut -d ':' -f 2)"
}

cpufmt () {
    echo -n "^ca(1,$SYSINFO)CPU: $1 ^ca()"
}

# This is my reconstructed version of the default panel that comes
# with herbstcluftwm, so I can figure out how it fits together
# and eventually build on it myself.

# First, we need to get the data to display which is the
# responsibility of the following code block. It prints
# to stdout in the format:
#
# <event>\t<data>
#
# which is then processed by the rest to get the panel.
{

    # Every second print the time, but send the loop to the
    # background so we can collect data from other sources
    while true ; do

        # Get the date
        date +$'date\t^fg(#ffffff)%d^fg()-%m-%Y ^fg(#ffffff)%H:%M^fg()'
        [ -x $BAT ] && $BAT
        cpuload

        sleep 1 || break

    done&

    datechild=$!

    # Test the internet
    (net_test && echo -e "net\tonline") || echo -e "net\toffline"

    # Get events from herbstclient, such as new focus, or
    # changed tag etc.
    herbstclient --idle

    # Cleanup
    kill $datechild
} 2> /dev/null | {

    # The triple "redirect" character tells bash that the
    # 'file' we are redirecting from is actually just a string
    IFS=$'\t' read -ra tags <<< $(herbstclient tag_status $monitor)
    sep="^fg($selbg)|^fg()"
    window=""
    err=""
    battery=""
    net=""
    cpu=""

    # The following code takes the input generated from the
    # previous code block and processes it to form the panel
    while true ; do

        # The special variable IFS tells bash what format the data
        # is written in by denoting the separator between fields
        #
        # The following read command, then reads in the data from
        # above into the array data, ready for us to process it
        IFS=$'\t' read -ra data || break

        # What type of data did we receive?
        case "${data[0]}" in
            tag*)
                IFS=$'\t' read -ra tags <<< $(herbstclient tag_status $monitor);;

            date)
                date=${data[@]:1};;

            cpu)
                cpu=$(cpufmt ${data[@]:1});;

            net)
                case ${data[1]} in
                    online)
                        net="^fg(#00ff00) online ^fg()";;
                    offline)
                        net="^fg(#ff0000) offline ^fg()";;
                esac;;

            power)
                battery=$(batfmt ${data[@]:1});;

            focus_changed|window_title_changed)
                window="^fg($highcolor)${data[2]}^fg()" ;;

            reload|quit_panel)
                exit;;

            *)
                err="^bg(#ff0000)^fg(#000000)Unknown event: ${data[0]}^fg()^bg()"
        esac

        # Now that we've processed the data, all that's left is to
        # do is produce the dzen output!

        # Draw the tags
        for t in "${tags[@]}" ; do
            tagfmt $t
        done

        # Write the window title - this is set above
        echo -n "${sep}${window}"

        # We will store what we want drawn on the right in a variable
        # which we will then do some fiddling with to get it to align
        # properly
        right="${cpu}${sep}${net}${sep}${battery}${sep}${date}${sep} "

        # Next we do a 'magic sed command' to extract the text without
        # all the dzen format commands
        #
        # sed 's.\^[^(]*([^)]*)..g'
        #   s.       The substitute command, note that you can actually use "any"
        #            character to deliniate the patterns so this uses dots rather
        #            than the conventional slash
        #
        #   \^       A literal ^
        #
        #   [^(]*    Match any character that is NOT a ( (zero or more times)
        #
        #   (        A literal (
        #
        #   [^)]*    Match any character that is NOT a ) (zero or more times)
        #
        #   ).       A literal ), (and end of search pattern
        #
        #   .g       An empty replacement pattern = delete, the g means replace
        #            all occurances on the line
        right_text=$(echo -n "$right" | sed 's.\^[^(]*([^)]*)..g')

        # Now that we have the text isolated, we can use the textwidth program
        # that comes with dzen to calculate how wide the right hand text will
        # be when rendered with a given font, to calculate the padding so that
        # it is right justified
        right_width=$(textwidth "$font" "$right_text    ")

        # With the width calculated, draw the write the right hand text
        # with the correct padding
        echo -n "^pa($(($panel_width - $right_width)))$right"

        # Write a newline to make dzen happy
        echo

    done

# Final step! Pipe the output into dzen
} 2> /dev/null | dzen2 -w $panel_width -x $x -y $y -h $panel_height \
          -bg "$bgcolor" -fn "$font" -ta l

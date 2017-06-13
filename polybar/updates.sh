#!/bin/bash

ping -q -w 2 -c 1 176.34.135.167 > /dev/null 2> /dev/null

if [[ "$?" == "0" ]] ; then

    pac=$(checkupdates | wc -l)
    aur=$(cower -u | wc -l)

    check=$((pac + aur))
    if [[ "$check" != "0" ]] ; then
        echo -n "$pac %{F#5b5b5b}%{F-} $aur | "
    fi

fi

cache=$(du -h /var/cache/pacman/pkg | cut -f 1)
num=$(ls /var/cache/pacman/pkg $(pacman -Q | awk '{printf "--ignore %s-%s* ", $1, $2}') | wc -l)

echo "$cache  $num"

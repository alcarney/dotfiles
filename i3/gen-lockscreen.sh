#!/bin/bash

# Inspired by the script linked below
# https://gitlab.com/formigoni/sys-admin/blob/master/xlock/lockscreen.sh

dir="$HOME/.xlock"
screen="$dir/lock.png"

scrot "$screen" || exit 1
convert "$screen" -paint 4 -grayscale Rec709Luminance "$screen" || exit 1

#i3lock -u -i "$screen" || exit 1

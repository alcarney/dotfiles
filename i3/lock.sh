#!/bin/bash

dir="$HOME/.xlock"
screen="$dir/screenshot.png"

scrot "$screen" || exit 1
convert "$screen" -paint 4 -grayscale Rec709Luminance "$screen"

i3lock -u -i "$screen"

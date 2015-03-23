#!/bin/sh
#
#	~/.xinitrc
#
# Executed by startx - window managers are launched from here

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# Programs common to many WMs
start_common ()
{
    xrdb -merge ~/.Xresources
    #redshift &
    compton&
}

# XMonad specfic stuff
start_xmonad()
{
    start_common
    xsetroot -cursor_name left_ptr
    feh --bg-fill "/home/alex/Media/Pictures/Wallpapers/bg.jpg"
    exec xmonad
}

# Default window manager
session=${1:-xmonad}

case $session in
    dwm) exec dwm;;
    xmonad) start_xmonad;;
    *) echo "Please enter an option"
esac

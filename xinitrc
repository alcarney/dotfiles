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
    xsetroot -cursor_name left_ptr
    feh --bg-fill "/home/alex/Media/Pictures/Wallpapers/bg.jpg"
    compton&
}

# XMonad specfic stuff
start_xmonad()
{
    start_common
    exec xmonad
}

start_dwm ()
{
    start_common    
    exec dwm
}


start_xfce ()
{
    xrdb -merge ~/.Xresources
    exec startxfce4
}

# Default window manager
session=${1:-xmonad}

case $session in
    dwm) start_dwm;;
    xfce) start_xfce;;
    xmonad) start_xmonad;;
    *) echo "Please enter an option"
esac

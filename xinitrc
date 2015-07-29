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

# Programs/settings common to many WMs
start_common ()
{
    # Use my .Xresources file
    xrdb -merge ~/.Xresources

    # Disable annoying beeps
    xset b off

    # Remap CAPS to ESC
    setxkbmap -option caps:escape

    # Disable those annoying "forward" and "back" keys on my laptop
    xmodmap -e "keycode 166="
    xmodmap -e "keycode 167="

    # Set the Cursor
    xsetroot -cursor_name left_ptr

    # Set the background and enable compositing
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

start_emacs ()
{
    start_common
    exec emacs
}

# Default window manager
session=${1:-xmonad}

case $session in
    emacs) start_emacs;;
    dwm) start_dwm;;
    xfce) start_xfce;;
    xmonad) start_xmonad;;
    *) echo "Please enter an option"
esac

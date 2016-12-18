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
    setxkbmap -layout gb -option caps:escape

    # Disable those annoying "forward" and "back" keys on my laptop
    xmodmap -e "keycode 166="
    xmodmap -e "keycode 167="

    # Set the Cursor
    xsetroot -cursor_name left_ptr

    # Set the background, enable compositing and notifications
    # feh --randomize --recursive --bg-scale "/home/alex/Media/Wallpapers/"
    feh --bg-scale '/home/alex/Media/Wallpapers/World_Trade_Flower.jpg'
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

start_i3 ()
{
    start_common
    polybar top&
    polybar bottom&
    exec i3
}

start_xfce ()
{
#    xrdb -merge ~/.Xresources
    exec startxfce4
}

start_emacs ()
{
    start_common
    exec emacs
}

start_bspwm()
{
    start_common
    sxhkd &
    exec bspwm
}

# Default window manager
session=${1:-i3}

case $session in
    bspwm) start_bspwm;;
    emacs) start_emacs;;
    dwm) start_dwm;;
    i3) start_i3;;
    xfce) start_xfce;;
    xmonad) start_xmonad;;
    *) echo "Please enter an option"
esac

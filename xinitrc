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

    # Use the previous wallpaper + colorscheme
    wal -R

    # Set the lockscreen program
    xss-lock -- i3lock -u -i $HOME/.xlock/lock.png &

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

start_herbs () {
    start_common
    exec herbstluftwm --locked
}

start_i3 ()
{
    start_common
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

start_mate()
{
    # Remap CAPS to ESC
    setxkbmap -layout gb -option caps:escape

    # Disable those annoying "forward" and "back" keys on my laptop
    xmodmap -e "keycode 166="
    xmodmap -e "keycode 167="
    exec mate-session
}

# Default window manager
session=${1:-i3}
case $session in
    bspwm) start_bspwm;;
    dwm) start_dwm;;
    hc) start_herbs;;
    mate) start_mate;;
    i3) start_i3;;
    *) echo "Please enter an option"
esac

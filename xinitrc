#! /bin/sh

# Not entirely sure what this does but it seems important.
if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# Use settings in the .Xresources file
xrdb -merge ~/.Xresources

# Disable beeping
xset b off

# Set GB layout, remap CAPS to ESC
setxkbmap -layout gb -option caps:escape

# Disable page up / page down keys on my laptop.
xmodmap -e "keycode 166="
xmodmap -e "keycode 167="

# Set the cursor
xsetroot -cursor_name left_ptr

# Start i3
exec i3

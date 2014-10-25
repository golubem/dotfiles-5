#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# set the cursor
  xsetroot -cursor_name left_ptr

# set background
  xsetroot -solid "#080808"

# start xmonad
  unclutter &
  exec dbus-launch startkde

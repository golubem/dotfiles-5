#!/bin/sh
if [ -d /etc/X11/xinit/xinitrc.d ]; then
for f in /etc/X11/xinit/xinitrc.d/*; do
[ -x "$f" ] && . "$f"
done
unset f
fi
xsetroot -cursor_name left_ptr &
xsetroot -solid "#080808" &
stalonetray &
unclutter &
nm-applet &
xfce4-power-manager &
start-pulseaudio-x11 &
xscreensaver -no-splash &
xfce4-volumed-pulse &
xfsettingsd &
xterm -e "/usr/bin/xset r rate 200 50" &
exec dbus-launch xmonad

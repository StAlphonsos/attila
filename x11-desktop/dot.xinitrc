#!/bin/sh
##
xset +fp /usr/local/lib/X11/fonts/webfonts
xset +fp /usr/local/lib/X11/fonts/URW
xset fp rehash
xrdb -merge $HOME/.Xdefaults
xmodmap $HOME/.xmodmaprc
xsetroot -solid black
openbox &
fbpanel
#mate-session

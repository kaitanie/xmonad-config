#!/bin/bash

if [ "$1" == "fi" ]; then
    /usr/bin/setxkbmap fi
fi

if [ "$1" == "gb" ]; then
    /usr/bin/setxkbmap gb
fi

xmodmap ~/.xmonad/xmodmap-settings

capitalizedname=$(echo $1 | tr [a-z] [A-Z])
notify-send -a "keymap.sh" "Current keymap:" "<b>$capitalizedname</b>"


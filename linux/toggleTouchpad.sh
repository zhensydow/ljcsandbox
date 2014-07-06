#!/bin/sh
sleep 1
TOUCHPAD_NAME=touchpad
DISPLAY=":0.0"
XAUTHORITY=/var/lib/mdm/:0.Xauth
export DISPLAY XAUTHORITY
tpID=$(xinput list | grep -i $TOUCHPAD_NAME | awk '{ print $6 }' | sed 's/id=//')
case "$1" in
   on)
   /usr/bin/xinput --enable $tpID
   ;;
   off)
   /usr/bin/xinput --disable $tpID
   ;;
esac

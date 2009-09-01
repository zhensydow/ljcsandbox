xrdb -merge .Xresources

trayer --edge top --align right --SetDockType true --SetPartialStrut true \
 --expand true --width 10 --transparent true --tint 0x191970 --height 12 &
 
wicd-client &
xpmroot /usr/share/backgrounds/simple-ubuntu.png &

xscreensaver -no-splash &


xmonad

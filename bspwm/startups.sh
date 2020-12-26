#!/bin/bash
# Start background processes in bspwm

sxhkd &
setxkbmap -option caps:escape &
/home/rob/dotfiles/polybar/launch.sh &
polybar bspwmbar & polybar bar2 &
gnome-keyring-daemon -s &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
start-pulseaudio-x11 &
ssh-agent &
/home/rob/.fehbg &
picom &
dunst &
xclip &
clipit &
recollindex -m &
trap 'kill $(jobs -p)' SIGINT SIGTERM EXIT

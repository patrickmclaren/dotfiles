#!/bin/bash

# getopts
# -------

# Skip startx appended parameter xterm
shift

# POSIX Variable
OPTIND=1 # reset in-case getopts has been previously used in the shell.

# Initialize variables
dpi_level="normal"
window_manager="dwm"

while getopts "d:m:" opt; do
    case $opt in
        d)
            dpi_level=$OPTARG
            ;;
        m)
            window_manager=$OPTARG
            ;;
    esac
done

# Setup display based on dpi_level
# --------------------------------

echo "DPI level: $dpi_level"
case "$dpi_level" in
    normal)
        echo "Loading Xresources"
        xrdb -load ~/.Xresources

        # Multi-head
        echo "Configuring multi-head display"
        xrandr --output eDP1 --mode 1920x1080 --output HDMI1 --mode 1920x1080 --scale 1x1 --right-of eDP1

        echo "Configuring GNOME scaling factor"
        gsettings set org.gnome.desktop.interface scaling-factor 1
        ;;
    hi)
        xrdb -load ~/.Xresources-hidpi

        # Single-head
        xrandr --output eDP1 --auto --output HDMI2 --off

        gsettings set org.gnome.desktop.interface scaling-factor 2
        ;;
esac

# Unconditional setup
# -------------------

xmodmap ~/.Xmodmap

emacs --daemon

nohup redshift -l 42.3993266:-71.0396939 -t 6500:3000 -b 1.0:0.5 >/dev/null 2>&1 &

urxvtd -q -f -o

# Window manager
# --------------

case "$window_manager" in
    dwm)
        compton -fb -D 5

        sh ~/.fehbg &

        dunst -follow keyboard -geometry 0-20-20 -fn "terminus 20" -key ctrl+BackSpace &

        while true; do
            xsetroot -name "$(date -I'minutes')"
            sleep 1m
        done &

        dbus-launch --exit-with-session dwm
        ;;
    gnome)
        exec gnome-session
        ;;
esac

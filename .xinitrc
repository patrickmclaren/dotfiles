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

# Unconditional setup
# -------------------

echo "Setting up keymaps"
xmodmap ~/.Xmodmap

echo "Starting `which emacs` daemon"
emacs --daemon

echo "Starting `which redshift-gtk` in background"
redshift-gtk &

echo "Starting `which urxvt` daemon"
urxvtd -q -f -o

pkill gpg-agent

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

    case "$window_manager" in
      dwm)
        echo "Starting `which dunst` notification daemon"
        dunst &
        ;;
      gnome)
        ;;
    esac
    ;;
  hi)
    echo "Loading Xresources-hidpi"
    xrdb -load ~/.Xresources-hidpi

    # Single-head
    echo "Configuring automatic displays"
    xrandr --output eDP1 --auto --output HDMI1 --auto

    echo "Configuring GNOME scaling factor"
    gsettings set org.gnome.desktop.interface scaling-factor 2

    case "$window_manager" in
      dwm)
        echo "Starting `which dunst` notification daemon"
        dunst -config $HOME/.config/dunst/dunstrc-hidpi &
        ;;
      gnome)
        ;;
    esac
    ;;
esac

# Window manager
# --------------

case "$window_manager" in
  dwm)
    echo "Starting `which compton` X11 compositor"
    compton -fb -D 5

    echo "Setting desktop background"
    sh ~/.fehbg &

    echo "Starting `which xscreensaver` in background"
    xscreensaver -no-splash &

    while true; do
      xsetroot -name "$(date -I'minutes')"
      sleep 1m
    done &

    dbus-launch --exit-with-session dwm
    ;;
  gnome)
    dbus-launch --exit-with-session gnome-session
    ;;
esac

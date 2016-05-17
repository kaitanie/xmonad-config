#!/bin/sh

xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation" 1
xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Button" 2
xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Timeout" 200
xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Axes" 6 7 4 5
xinput set-prop "TPPS/2 IBM TrackPoint" "Device Accel Constant Deceleration" 0.75
 
ON_UNICOMP=$(xinput list | grep -i "Unicomp  Endura Pro Keyboard" | grep pointer)
echo "ON_UNICOMP = $ON_UNICOMP"

if [ -n "$ON_UNICOMP" ]; then
    echo "Using Unicomp..."
    # xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation" 1
    # xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Button" 3
    # xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Timeout" 200
    # xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Axes" 6 7 4 5
    # xinput set-prop "TPPS/2 IBM TrackPoint" "Device Accel Constant Deceleration" 0.75    e
else
    echo "Not using Unicomp..."
fi

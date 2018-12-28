#!/bin/bash

# Polybar startup script taken from the github wiki

killall -q polybar

while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

polybar top&
polybar bottom&

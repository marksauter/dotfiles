#!/usr/bin/sh

# Terminate already running bar instance
killall -q polybar

# wait until the processes have been shut down
while pgrep -x polybar >/dev/null; do sleep 1; done

# launch polybar
polybar top &

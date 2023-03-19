#!/bin/bash

muted=$(pactl get-sink-mute @DEFAULT_SINK@ | awk '{print $2}')

if [[ "$muted" == "yes" ]]; then
	echo "MUTED"
else
	echo "ENABLED"
fi

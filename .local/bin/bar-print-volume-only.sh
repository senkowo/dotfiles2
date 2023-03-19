#!/bin/bash

#volume=$(pactl get-sink-volume @DEFAULT_SINK@ | awk '{print $5}' | sed s/%//)
volume=$(pactl get-sink-volume @DEFAULT_SINK@ | awk '{print $5}')

echo $volume

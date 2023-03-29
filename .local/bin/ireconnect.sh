#!/usr/bin/env sh

name=$(iwctl station wlan0 show | awk '$1" "$2=="Connected network" {print $3}')

if [ -z "$name" ];
then
    echo "Not connected to network, cannot reconnect."
    exit 1
fi

iwctl station wlan0 disconnect

iwctl station wlan0 connect $name

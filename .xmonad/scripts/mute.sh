#!/bin/sh

mute=`amixer get Speaker | awk -F'[]%[]' '/%/ {if ($7 == "off") { print "(H)" } else { print "(D)" }}' | head -n 1`

echo $mute

exit 0

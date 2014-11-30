#!/bin/sh

iwconfig wlan0 2>&1 | grep -q no\ wireless\ extensions\. && {
  echo wired
  exit 0
}

stngth=`/sbin/iwconfig wlan0 | awk -F '=' '/Quality/ {print $2}' | cut -d '/' -f 1`

echo $stngth%

exit 0

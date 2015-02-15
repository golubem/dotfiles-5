if [ "`xset -q | awk -F \" \" '/Group 2/ {print($4)}'`" = "on" ]; then
    layout="RU"
else
    layout="EN"
fi

echo $layout

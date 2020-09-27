#!/bin/sh

echo foo bar baz

for i in $(seq 1 100)
do amixer sset Master 1%+
sleep 0.03
amixer sset Master 1%-
sleep 0.03
done >/dev/null &

for i in $(seq 1 100)
do xbacklight +1
sleep 0.03
xbacklight -1
sleep 0.03
done >/dev/null &

echo going for a few seconds...

sleep 15

echo almost done...

sleep 3

echo done

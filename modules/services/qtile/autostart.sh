#!/usr/bin/env bash

scripts=$(ls -1 ~/.config/qtile/autostart.d/*.sh)
for i in $scripts
do
    source $i
done
echo "Finished qtile autostart"

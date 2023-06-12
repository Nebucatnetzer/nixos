#!/usr/bin/env bash
echo "start signal"
nohup signal-desktop --use-tray-icon --no-sandbox > ~/.local/share/nohup.out &

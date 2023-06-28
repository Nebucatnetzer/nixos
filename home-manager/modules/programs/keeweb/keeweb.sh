#!/usr/bin/env bash
echo "start keeweb"
nohup keeweb --in-process-gpu > ~/.local/share/nohup.out &

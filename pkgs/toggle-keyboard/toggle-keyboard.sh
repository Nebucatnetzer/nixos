#!/usr/bin/env bash

toggle_keyboard() {
    if pgrep "evtest"; then
        sudo pkill evtest
        printf "Internal keyboard successfully attached.\n"
    else
        sudo evtest --grab /dev/input/event1 >/dev/null &
        printf "Internal keyboard successfully detached.\n"
    fi
}
toggle_keyboard

#!/usr/bin/env bash

find_internal_keyboard() {
    xinput list --id-only 'AT Translated Set 2 keyboard'
}
find_master_keyboard() {
    xinput list --id-only 'Virtual core keyboard'
}

toggle_keyboard() {
    if xinput | grep "Translated Set 2 keyboard" | grep -qv "floating"; then
        internal_keyboard=$(find_internal_keyboard)
        xinput float "$internal_keyboard"
        printf "Internal keyboard successfully detached.\n"
    else
        internal_keyboard=$(find_internal_keyboard)
        master_keyboard=$(find_master_keyboard)
        xinput reattach "$internal_keyboard" "$master_keyboard"
        printf "Internal keyboard successfully attached.\n"
    fi
}
toggle_keyboard

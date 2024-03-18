#!/usr/bin/env bash
function replace-listings() {
    sed -i -e 's/end{lstlisting}/end{sexylisting}/g' $1
    for fn in $(grep 'begin{lstlisting}' $1); do
        caption=$(grep -m 1 -B 1 -P 'begin{lstlisting}' $1 |
            grep -oP 'caption={\K[^\}]+')
        sed -i "0,/begin{lstlisting}/ s/{lstlisting}/{sexylisting}{$caption}/" $1
    done
}
replace-listings $1
exit 0

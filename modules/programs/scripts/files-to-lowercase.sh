#!/usr/bin/env bash
for i in $(ls | grep [A-Z]); do mv -i $i $(echo $i | tr 'A-Z' 'a-z'); done

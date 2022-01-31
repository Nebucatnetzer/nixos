#!/usr/bin/env bash

find ./secrets -type d -print0 | xargs -0 chmod 700
find ./secrets -type f -print0 | xargs -0 chmod 600

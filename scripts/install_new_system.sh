#!/usr/bin/env bash

host=$1
fqdn="nixos@nixos.2li.local"
nixos-rebuild switch --use-remote-sudo --build-host localhost --target-host $fqdn --impure --flake ".#$host"

#!/usr/bin/env bash

host=$1
fqdn="nixos@nixos.2li.local"
nixos-rebuild switch --use-remote-sudo --target-host $fqdn --impure --flake ".#$host" |& nom

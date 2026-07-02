hosts_str=$(nix eval "$DEVENV_ROOT"#nixosConfigurations \
    --apply 'pkgs: builtins.concatStringsSep " " (builtins.attrNames pkgs)')
hosts_str=${hosts_str//\"/}
read -ra hosts <<< "$hosts_str"
skip=(
    "gwyn"
    "test-raspi"
)

for host in "${hosts[@]}"; do
    if [[ " ${skip[*]} " == *" ${host} "* ]]; then
        continue
    fi
    echo "$host"
    nixos-rebuild-ng dry-build --flake "$DEVENV_ROOT#$host"
    echo
    echo
done

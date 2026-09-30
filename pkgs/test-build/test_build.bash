PROJECT_ROOT=$(git rev-parse --show-toplevel)
hosts_str=$(
    nix eval "$PROJECT_ROOT#nixosConfigurations" \
        --apply 'pkgs: builtins.concatStringsSep " " (builtins.attrNames pkgs)'
)
hosts_str=${hosts_str//\"/}
read -ra hosts <<<"$hosts_str"
skip=(
    "test-raspi"
)

for host in "${hosts[@]}"; do
    if [[ " ${skip[*]} " == *" ${host} "* ]]; then
        continue
    fi
    echo "$host"
    nixos-rebuild dry-build --flake "$PROJECT_ROOT#$host"
    echo
    echo
done

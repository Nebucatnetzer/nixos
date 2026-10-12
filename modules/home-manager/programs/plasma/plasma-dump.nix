{
  git,
  lib,
  nixfmt,
  rc2nix,
  writeShellApplication,
}:
writeShellApplication {
  name = "plasma-dump";
  runtimeInputs = [
    git
    nixfmt
    rc2nix
  ];
  meta = {
    description = "Dump the current Plasma config with rc2nix and commit it";
    license = lib.licenses.gpl3Plus;
    mainProgram = "plasma-dump";
    platforms = lib.platforms.linux;
  };
  text = ''
    repository="$HOME/.nixos"
    config_file="modules/home-manager/programs/plasma/config.nix"

    # Generate first, so that a failed rc2nix run does not empty the file.
    # rc2nix prints one entry per line. The filter drops entries with file names,
    # search terms and drive labels, so that they do not end up in git.
    generated_config=$(
      rc2nix \
        | grep --invert-match --fixed-strings --file=${./plasma-dump-filter.txt} \
        | sed --regexp-extended --file=${./plasma-dump.sed} \
        | nixfmt -
    )
    printf '%s\n' "$generated_config" >"$repository/$config_file"

    if [[ -z $(git -C "$repository" status --porcelain -- "$config_file") ]]; then
      echo "The Plasma config has not changed."
      exit 0
    fi

    git -C "$repository" add -- "$config_file"
    # Commit only the config file, even if other changes are staged.
    git -C "$repository" commit --message "Update the Plasma config" -- "$config_file"
  '';
}

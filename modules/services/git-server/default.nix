{
  repoRoot ? "/var/lib/git",
}:
{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  # restrict: no pty and no forwarding. git-shell already limits the commands.
  authorizedKeys = map (
    key: "restrict ${key}"
  ) config.users.users.${config.az-username}.openssh.authorizedKeys.keys;
  emptyHooks = pkgs.runCommand "git-hooks-empty" { } "mkdir $out";
  hasActions = repo: (repo.pushMirrors or [ ]) != [ ];
  hooksFor = name: repo: if hasActions repo then spoolHooks name else emptyHooks;
  initCalls = lib.concatStrings (
    lib.mapAttrsToList (name: repo: ''
      init_repo ${lib.escapeShellArg name} ${
        lib.escapeShellArg (repo.description or name)
      } ${hooksFor name repo}
    '') repos
  );
  repos = import ./repos.nix;
  spoolDirectory = "${repoRoot}/.spool";
  # Append, not overwrite, so a second push before the service runs keeps its ref lines.
  spoolHooks =
    name:
    pkgs.writeTextFile {
      name = "git-hooks-${slug name}";
      destination = "/post-receive";
      executable = true;
      text = ''
        #!${pkgs.runtimeShell}
        cat >> ${spoolDirectory}/${slug name}
      '';
    };
  slug = name: lib.replaceStrings [ "/" ] [ "-" ] name;
in
{
  users.groups.git = { };
  users.users.git = {
    isSystemUser = true;
    group = "git";
    home = repoRoot;
    shell = "${pkgs.git}/bin/git-shell";
    openssh.authorizedKeys.keys = authorizedKeys;
  };

  # RemainAfterExit: a rebuild that changes the repo set restarts the unit.
  systemd.services.git-repos-init = {
    description = "Create the declared bare git repositories";
    wantedBy = [ "multi-user.target" ];
    after = [ "systemd-tmpfiles-setup.service" ];
    path = [ pkgs.git ];
    serviceConfig = {
      Type = "oneshot";
      User = "git";
      Group = "git";
      RemainAfterExit = true;
    };
    script = ''
      init_repo() {
        local name="$1"
        local description="$2"
        local repo_path="${repoRoot}/$name.git"
        if [ ! -d "$repo_path" ]; then
          mkdir --parents "$(dirname "$repo_path")"
          git init --bare "$repo_path"
        fi
        printf '%s\n' "$description" > "$repo_path/description"
        git --git-dir="$repo_path" config core.hooksPath "$3"
      }

      ${initCalls}
    '';
  };

  systemd.tmpfiles.rules = [
    "d ${repoRoot} 0750 git git -"
    "d ${spoolDirectory} 0750 git git -"
  ];

  # Must stay last in sshd_config. A Match list needs the leading * to match anything.
  services.openssh.extraConfig = lib.mkAfter ''
    Match Address *,!10.7.89.0/24,!10.70.89.0/24,!127.0.0.0/8
      AllowUsers git
  '';
}

{
  domain ? "git.zweili.org",
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
  authorizedKeys = map (key: "restrict ${key}") (
    config.users.users.${config.az-username}.openssh.authorizedKeys.keys ++ gitOnlyKeys
  );
  emptyHooks = pkgs.runCommand "git-hooks-empty" { } "mkdir $out";
  # These keys may only use git. They cannot log in to any host.
  gitOnlyKeys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK0EtKlimFd56pvvHr+witH8ru5suizw4MDnIbRqEy/t zweili@CO-NB-102"
  ];

  hasActions = repo: (repo.pushMirrors or [ ]) != [ ];

  hooksFor = name: repo: if hasActions repo then spoolHooks name else emptyHooks;
  initCalls = lib.concatStrings (
    lib.mapAttrsToList (name: repo: ''
      init_repo ${lib.escapeShellArg name} ${
        lib.escapeShellArg (repo.description or name)
      } ${hooksFor name repo}
    '') repos
  );
  hookPaths = lib.mapAttrs' (
    name: repo:
    lib.nameValuePair "git-hook-${slug name}" {
      wantedBy = [ "multi-user.target" ];
      pathConfig.PathExists = "${spoolDirectory}/${slug name}";
    }
  ) mirroredRepos;
  # The spool file goes first. post-receive runs after the refs are updated, so a push
  # during this run is either in this push or starts the service again.
  # Only branches and tags: GitHub rejects other refs, for example gitea's refs/pull/*.
  hookService = name: repo: {
    description = "Push ${name} to its mirrors";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    onFailure = [ "unit-status-telegram@%N.service" ];
    path = [
      pkgs.git
      pkgs.openssh
    ];
    environment.GIT_SSH_COMMAND = "ssh -i ${config.age.secrets.githubMirrorKey.path} -o IdentitiesOnly=yes -o StrictHostKeyChecking=yes";
    serviceConfig = {
      Type = "oneshot";
      User = "git";
      Group = "git";
    };
    script = ''
      cat ${spoolDirectory}/${slug name}
      rm ${spoolDirectory}/${slug name}
    ''
    + lib.concatMapStrings (remote: ''
      git --git-dir=${repoRoot}/${name}.git push --prune ${lib.escapeShellArg remote} \
        '+refs/heads/*:refs/heads/*' '+refs/tags/*:refs/tags/*'
    '') repo.pushMirrors;
  };
  hookServices = lib.mapAttrs' (
    name: repo: lib.nameValuePair "git-hook-${slug name}" (hookService name repo)
  ) mirroredRepos;

  # RemainAfterExit: a rebuild that changes the repo set restarts the unit.
  initService = {
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

  # The start page and old gitea links point visitors to the GitHub mirrors.
  landingPage = pkgs.writeTextDir "index.html" ''
    <!doctype html>
    <html lang="en">
    <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>${domain}</title>
    </head>
    <body>
    <h1>${domain}</h1>
    <p>This server used to have a web interface.
    Aggressive LLM crawlers forced me to remove it.
    The public repositories moved to GitHub.
    </p>
    <p>These repositories have a mirror on GitHub:</p>
    <ul>
    ${mirrorList}
    </ul>
    <p>All projects: <a href="https://github.com/Nebucatnetzer">github.com/Nebucatnetzer</a></p>
    </body>
    </html>
  '';
  mirrorList = lib.concatStrings (
    lib.mapAttrsToList (name: repo: ''
      <li><a href="${mirrorWebUrl (lib.head repo.pushMirrors)}">${lib.escapeXML name}</a>${
        lib.optionalString (repo ? description) ": ${lib.escapeXML repo.description}"
      }</li>
    '') mirroredRepos
  );
  # Case-insensitive, because gitea URLs were.
  mirrorRedirects = lib.mapAttrs' (
    name: repo:
    lib.nameValuePair "~* ^/${lib.escapeRegex name}(/|$)" {
      return = "302 ${mirrorWebUrl (lib.head repo.pushMirrors)}";
    }
  ) mirroredRepos;
  mirrorWebUrl =
    remote:
    lib.removeSuffix ".git" (lib.replaceStrings [ "git@github.com:" ] [ "https://github.com/" ] remote);

  mirroredRepos = lib.filterAttrs (name: repo: hasActions repo) repos;
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
  imports = [
    "${inputs.self}/modules/services/nginx-acme-base"
    "${inputs.self}/modules/services/telegram-notifications"
  ];

  age.secrets.githubMirrorKey = {
    file = "${inputs.self}/scrts/gwyn_github.key.age";
    mode = "400";
    owner = "git";
    group = "git";
  };

  programs.ssh.knownHosts.github = {
    hostNames = [ "github.com" ];
    publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl";
  };

  # Must stay last in sshd_config. A Match list needs the leading * to match anything.
  services.openssh.extraConfig = lib.mkAfter ''
    Match Address *,!10.7.89.0/24,!10.70.89.0/24,!127.0.0.0/8
      AllowUsers git
  '';

  services.nginx.virtualHosts.${domain} = {
    enableACME = true;
    forceSSL = true;
    root = landingPage;
    locations = mirrorRedirects;
  };

  systemd.paths = hookPaths;
  systemd.services = hookServices // {
    git-repos-init = initService;
  };
  systemd.tmpfiles.rules = [
    "d ${repoRoot} 0750 git git -"
    "d ${spoolDirectory} 0750 git git -"
  ];

  users.groups.git = { };
  users.users.git = {
    isSystemUser = true;
    group = "git";
    home = repoRoot;
    shell = "${pkgs.git}/bin/git-shell";
    openssh.authorizedKeys.keys = authorizedKeys;
  };
}

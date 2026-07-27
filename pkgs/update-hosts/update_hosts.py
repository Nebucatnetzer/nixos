import argparse
import logging
import os
import subprocess
import sys
from pathlib import Path


def get_hosts(devenv_root: str) -> list[str]:
    """Get the list of hostnames from the specified Nix flake.

    Args:
        devenv_root: The path to the flake root.

    Returns:
        A list of hostnames.
    """
    expr = f"{devenv_root}#nixosConfigurations"
    apply_expr = 'pkgs: builtins.concatStringsSep " " (builtins.attrNames pkgs)'
    cmd = ["nix", "eval", expr, "--apply", apply_expr]
    try:
        result = subprocess.run(cmd, check=True, capture_output=True, text=True)
    except subprocess.CalledProcessError as error:
        print(error.stderr)
        sys.exit(error.returncode)
    hosts_str = result.stdout.strip().strip('"')
    return hosts_str.split()


def is_skipped(devenv_root: str, host: str) -> bool:
    """Check whether a host is marked as skippable.

    A host is skipped when a `.skip-deploy` marker file exists in its
    `systems/<host>/` directory. The file's contents are irrelevant.

    Args:
        devenv_root: The path to the flake root.
        host: The hostname to check.

    Returns:
        True if the host should be skipped, False otherwise.
    """
    marker: Path = Path(devenv_root) / "systems" / host / ".skip-deploy"
    return marker.exists()


def deploy(devenv_root: str, host: str, subcommand: str) -> bool:
    """Deploy a single host with nixos-rebuild.

    Args:
        devenv_root: The path to the flake root.
        host: The hostname to deploy.
        subcommand: The nixos-rebuild subcommand (e.g. 'switch' or 'boot').

    Returns:
        True if the deployment succeeded, False otherwise.
    """
    logger = logging.getLogger(__name__)
    fqdn: str = f"{host}.vpn.zweili.org"
    logger.info("%s via %s", fqdn, subcommand)
    result = subprocess.run(
        [
            "nixos-rebuild",
            subcommand,
            "-j",
            "auto",
            "--sudo",
            "--target-host",
            fqdn,
            "--flake",
            f"{devenv_root}#{host}",
        ],
        check=False,
    )
    return result.returncode == 0


def main() -> None:
    logger = logging.getLogger(__name__)
    logging.basicConfig(level=logging.INFO, format="%(message)s")

    rsa_key: Path = Path("~/.nixos/secrets/ssh_keys/ansible/ansible.key").expanduser()
    os.environ["NIX_SSHOPTS"] = f"-i {rsa_key}"

    parser: argparse.ArgumentParser = argparse.ArgumentParser(
        description="Remote NixOS deployment tool for one or more hosts",
    )
    parser.add_argument(
        "hosts",
        nargs="*",
        help="Target hostnames (defaults to all hosts in the flake)",
    )
    parser.add_argument(
        "-r",
        "--reboot",
        action="store_true",
        help="Require a reboot after the update",
    )
    args: argparse.Namespace = parser.parse_args()

    devenv_root: str = os.environ["DEVENV_ROOT"]
    subcommand: str = "boot" if args.reboot else "switch"

    hosts: list[str] = args.hosts if args.hosts else get_hosts(devenv_root)

    succeeded: list[str] = []
    skipped: list[str] = []
    failed: list[str] = []

    for host in hosts:
        if is_skipped(devenv_root, host):
            logger.info("Skipping %s (.skip-deploy present)", host)
            skipped.append(host)
            continue

        if deploy(devenv_root, host, subcommand):
            succeeded.append(host)
        else:
            logger.error("Deployment of %s failed", host)
            failed.append(host)

    logger.info("")
    logger.info("Summary:")
    logger.info("  succeeded: %s", " ".join(succeeded) or "-")
    logger.info("  skipped:   %s", " ".join(skipped) or "-")
    logger.info("  failed:    %s", " ".join(failed) or "-")

    if failed:
        sys.exit(1)


if __name__ == "__main__":
    main()

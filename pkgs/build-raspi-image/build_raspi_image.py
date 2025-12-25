"""Build images for Raspberry Pis."""

import getpass
import os
import subprocess
import sys
import tempfile
from pathlib import Path


def _get_flake_root() -> Path:
    """Find the root of the git repository."""
    try:
        git_root = subprocess.run(
            [
                "git",
                "rev-parse",
                "--show-toplevel",
            ],
            capture_output=True,
            check=True,
            encoding="utf-8",
            text=True,
        ).stdout.strip("\n")
    except subprocess.CalledProcessError as error:
        print(error.stderr)
        sys.exit(error.returncode)
    return Path(git_root)


def _get_hosts(flake_root: Path) -> list[str]:
    """Return a list of all NixOS hosts."""
    expr = f"{flake_root}#nixosConfigurations"
    apply_expr = 'pkgs: builtins.concatStringsSep " " (builtins.attrNames pkgs)'
    cmd = ["nix", "eval", expr, "--apply", apply_expr]
    try:
        result = subprocess.run(cmd, check=True, capture_output=True, text=True)
    except subprocess.CalledProcessError as error:
        print(error.stderr)
        sys.exit(error.returncode)
    hosts_str = result.stdout.strip().strip('"')
    return hosts_str.split()


def _get_pis(flake_root: Path, hosts: str) -> [str]:
    """Return a list of all hosts with aarch64-linux archicture."""
    raspberry_pis = []
    for host in hosts:
        expr = (
            f"{flake_root}#nixosConfigurations.{host}.pkgs.stdenv.hostPlatform.system"
        )
        cmd = ["nix", "eval", expr]
        try:
            result = subprocess.run(cmd, check=True, capture_output=True, text=True)
            if "aarch64-linux" in result.stdout.strip().strip('"').strip("\n"):
                raspberry_pis.append(host)
        except subprocess.CalledProcessError as error:
            print(error.stderr)
            sys.exit(error.returncode)
    return raspberry_pis


def ask_for_hostname(flake_root: Path) -> [str]:
    """Present a list of hosts to choose from."""
    all_hosts = _get_hosts(flake_root)
    pis = _get_pis(flake_root, all_hosts)
    print("\n".join(f"{number}: {host}" for number, host in enumerate(pis)))
    selection = int(input("Which image would you like to build?: "))
    return pis[selection]


def generate_build_script(flake_root: Path, hostname: str) -> Path:
    """Generate the image build script for the chosen host."""
    try:
        print(f"Generate build script for {hostname}")
        image_script = subprocess.run(
            [
                "nix",
                "build",
                "--print-out-paths",
                "--no-link",
                f"{flake_root}#nixosConfigurations.{hostname}.config.system.build.diskoImagesScript",
            ],
            check=True,
            capture_output=True,
            encoding="utf-8",
        ).stdout
    except subprocess.CalledProcessError as error:
        print(error.stderr)
        sys.exit(error.returncode)
    return Path(image_script.strip("\n"))


def generate_initrd_ssh_key() -> None:
    """Generate the SSH key required for remote unlocking LUKS."""
    initrd_ssh_key = "/tmp/ssh_host_ed25519_key"
    try:
        print("Generate initrd SSH key.")
        # Remove the key first if it still exists otherwise the commands waits for input.
        Path(initrd_ssh_key).unlink(missing_ok=True)
        subprocess.run(
            [
                "ssh-keygen",
                "-t",
                "ed25519",
                "-N",
                "",
                "-C",
                "",
                "-f",
                initrd_ssh_key,
            ],
            check=True,
            capture_output=True,
            encoding="utf-8",
        )
    except subprocess.CalledProcessError as error:
        print(error.stderr)
        sys.exit(error.returncode)


def build_image(build_script: Path, password_file: Path) -> None:
    """Build the actual image."""
    try:
        print("Building image.")
        subprocess.run(
            [
                build_script,
                "--pre-format-files",
                "/tmp/ssh_host_ed25519_key",
                "/tmp/ssh_host_ed25519_key",
                "--pre-format-files",
                password_file,
                "/tmp/luks_password.key",
            ],
            check=True,
        )

        os.system("clear")
        print("\n\nSuccessfully finished build the image.\n")
    except subprocess.CalledProcessError as error:
        print(error.stderr)
        sys.exit(error.returncode)


def main() -> None:
    """Run the main logic of the script."""
    flake_root = _get_flake_root()
    hostname = ask_for_hostname(flake_root)
    luks_password1 = getpass.getpass(prompt="\nPlease provide the LUKS password: ")
    luks_password2 = getpass.getpass(prompt="Please enter it again: ")
    if luks_password1 != luks_password2:
        print("The passwords don't match, aborting.")
        sys.exit(1)
    generate_initrd_ssh_key()
    build_script = generate_build_script(flake_root, hostname)
    with tempfile.NamedTemporaryFile(mode="w+", encoding="utf-8") as password_file:
        password_file.write(f"{luks_password1}\n")
        password_file.flush()
        build_image(build_script, password_file.name)


if __name__ == "__main__":
    main()

{
  fetchFromGitHub,
  lib,
  rustPlatform,
}:
rustPlatform.buildRustPackage (finalAttrs: {
  pname = "minimoon-sync-server";
  version = "0.4.0";

  __structuredAttrs = true;
  strictDeps = true;

  src = fetchFromGitHub {
    owner = "milesegan";
    repo = "minimoon-sync-server";
    tag = "v${finalAttrs.version}";
    hash = "sha256-ffggj/3Y61j+moFvL2r86B1333BQsWrIpxFmpYd1qLk=";
  };

  cargoHash = "sha256-owxL9uqmgYdavSVQQk7Er6jLpfCzo+tcyUzIXwwmzxU=";

  # The Nix sandbox has no default IPv4 route, so this test cannot find a LAN address.
  checkFlags = [
    "--skip=server::tests::run_server_serves_files_on_advertised_and_loopback_addresses"
  ];

  meta = {
    description = "LAN file server for the Wi-Fi sync of the MiniMoon iOS music player";
    homepage = "https://github.com/milesegan/minimoon-sync-server";
    license = lib.licenses.mit;
    mainProgram = "minimoon-sync-server";
    platforms = lib.platforms.linux;
  };
})

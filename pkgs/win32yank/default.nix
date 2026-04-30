{
  lib,
  fetchFromGitHub,
  pkgsCross,
}:

let
  win = pkgsCross.mingwW64.rustPlatform;
in
win.buildRustPackage {
  pname = "win32yank";
  version = "0-unstable-2025-04-30";

  src = fetchFromGitHub {
    owner = "tk555";
    repo = "win32yank";
    rev = "3d8208498325a5fb82b73ef340c06c5ff04d84b5";
    hash = "sha256-0c4BQJrcCo3uVWbW88rFDbS6/0p8eBBMVlh+bZ4xgf8=";
  };

  cargoHash = "sha256-WIUHZQy7hxgJe5Vq0aJN7KzTiRqUCz5Ghrovxf/bjtQ=";

  meta = with lib; {
    description = "Clipboard tool for Windows/WSL2 interop";
    homepage = "https://github.com/equalsraf/win32yank";
    license = licenses.mit;
  };
}

{
  lib,
  stdenv,
  fetchFromGitHub,
  autoPatchelfHook,
}:

stdenv.mkDerivation rec {
  pname = "dptfxtract";
  version = "1.4.3"; # Latest official release tag

  src = fetchFromGitHub {
    owner = "intel";
    repo = "dptfxtract";
    rev = "v${version}";
    hash = "sha256-1r4DaMNW4USnEFD33K1/sVro674SQEzcIaGoZ4zjwcE=";
  };

  nativeBuildInputs = [ autoPatchelfHook ];

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall

    # Install the pre-compiled binary into the output bin directory
    install -Dm755 dptfxtract -t $out/bin/

    runHook postInstall
  '';

  meta = with lib; {
    description = "Intel DPTF Extract Utility for generating thermald's thermal_conf.xml";
    homepage = "https://github.com/intel/dptfxtract";
    # The repository is publicly distributed by Intel, but the binary itself is proprietary
    license = licenses.unfreeRedistributable;
  };
}

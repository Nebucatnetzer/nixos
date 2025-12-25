{
  fetchzip,
  python3Packages,
  stdenvNoCC,
}:
let
  version = "1.50";
in
stdenvNoCC.mkDerivation {
  inherit version;
  pname = "rpi4-uefi-firmware-images";
  src = fetchzip {
    url = "https://github.com/pftf/RPi4/releases/download/v${version}/RPi4_UEFI_Firmware_v${version}.zip";
    hash = "sha256-g8046/Ox0hZgvU6u3ZfC6HMqoTME0Y7NsZD6NvUsp7w=";
    stripRoot = false;
  };
  nativeBuildInputs = [
    python3Packages.virt-firmware
  ];
  dontFixUp = true;

  installPhase = ''
    runHook preInstall
    mkdir $out
    rm Readme.md

    # Remove the RAM limitation to allow the usage of the full 8 GB
    virt-fw-vars \
        --input RPI_EFI.fd \
        --output RPI_EFI.fd \
        --set-json <(cat <<EOF
    {
        "version": 2,
        "variables": [
            {
                "name": "RamLimitTo3GB",
                "guid": "cd7cc258-31db-22e6-9f22-63b0b8eed6b5",
                "data": "00000000"
            }
        ]
    }
    EOF
    )

    cp -r ./. $out
    runHook postInstall
  '';
}

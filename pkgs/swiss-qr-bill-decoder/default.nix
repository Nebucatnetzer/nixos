{
  fetchFromGitHub,
  ghostscript,
  lib,
  libnotify,
  rustPlatform,
  wl-clipboard,
  writers,
  writeShellApplication,
}:
let
  qr-bill-to-spc = writers.writePython3Bin "qr-bill-to-spc" {
    flakeIgnore = [
      "E501"
    ];
  } (builtins.readFile ./qr_bill_to_spc.py);

  swiss-qr-bill-decoder = rustPlatform.buildRustPackage (finalAttrs: {
    pname = "swiss-qr-bill-decoder";
    version = "1.3.0";

    __structuredAttrs = true;
    strictDeps = true;

    src = fetchFromGitHub {
      owner = "smartive";
      repo = "swiss-qr-bill-decoder";
      tag = "v${finalAttrs.version}";
      hash = "sha256-bnjWlUF6aRwda/VjjJqGlGmrgIbmF4Y5Ge9CQ1fBgQY=";
    };

    cargoHash = "sha256-o7Iq2mt5MN2rnE627dzWuU4B1kRuDv3D+DPbwwuK9sc=";

    meta = {
      description = "Decode a Swiss QR bill from an image or PDF into JSON";
      homepage = "https://github.com/smartive/swiss-qr-bill-decoder";
      license = lib.licenses.mit;
      mainProgram = "swiss-qr-bill-decoder";
      platforms = lib.platforms.linux;
    };
  });
in
writeShellApplication {
  name = "swiss-qr-bill-decoder";
  runtimeInputs = [
    ghostscript # 'gs' is invoked at runtime to rasterise PDF input
    libnotify
    qr-bill-to-spc
    wl-clipboard
  ];
  text = ''
    stderr_file=$(mktemp)
    trap 'rm -f "$stderr_file"' EXIT

    notify_error() {
      notify-send --app-name="Swiss QR Bill Decoder" --icon=dialog-error --urgency=critical \
        "Could not decode QR bill" "$1"
    }

    # Suspend errexit around each step so a failure lands in its error branch
    # instead of aborting the wrapper before we can report it.
    set +e
    json=$(${lib.getExe swiss-qr-bill-decoder} "$@" 2>"$stderr_file")
    status=$?
    set -e
    if [ "$status" -ne 0 ]; then
      notify_error "$(cat "$stderr_file")"
      exit "$status"
    fi

    set +e
    spc=$(printf '%s' "$json" | qr-bill-to-spc 2>"$stderr_file")
    status=$?
    set -e
    if [ "$status" -ne 0 ]; then
      notify_error "$(cat "$stderr_file")"
      exit "$status"
    fi

    printf '%s' "$spc" | wl-copy
    notify-send --app-name="Swiss QR Bill Decoder" --icon=view-barcode-qr \
      "QR bill decoded" "The payment code was copied to the clipboard."
  '';
}

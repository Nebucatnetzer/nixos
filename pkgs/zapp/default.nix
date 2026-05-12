{
  fetchFromGitHub,
  lib,
  pkg-config,
  rustPlatform,
  systemd,
}:

rustPlatform.buildRustPackage (finalAttrs: {
  pname = "zapp";
  version = "1.0.1";

  __structuredAttrs = true;
  strictDeps = true;

  src = fetchFromGitHub {
    owner = "zsa";
    repo = "zapp";
    tag = "v${finalAttrs.version}";
    hash = "sha256-KhWL+SsN1z9qpxwHpaqRo3qAk7xAOHVkRAOa02Q2Myc=";
  };

  cargoHash = "sha256-gDyNwHrMdNQdKdr9RGfwFAU8IaUlGrlJxV0WClQ25JM=";
  buildInputs = [
    systemd
  ];
  nativeBuildInputs = [
    pkg-config
  ];
  doInstallCheck = true;

  meta = {
    description = "Flash ZSA keyboards from your terminal";
    homepage = "https://github.com/zsa/zapp";
    license = [
      lib.licenses.commons-clause
      lib.licenses.mit
    ];
    mainProgram = "zapp";
  };
})

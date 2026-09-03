{
  lib,
  appimageTools,
  fetchurl,
  nghttp2,
}:
let
  pname = "unsloth-studio";
  version = "0.1.806-beta";
  src = fetchurl {
    url = "https://github.com/unslothai/unsloth/releases/download/v${version}/Unsloth-Desktop-Linux.AppImage";
    hash = "sha256-Qm9q0GaxuxkcFJfwlLXZy2r9B1QUmb2OJrMdpeiWyBY=";
  };
  contents = appimageTools.extract { inherit pname version src; };
in
appimageTools.wrapType2 {
  inherit pname version src;

  # The bundle ships its own GTK/WebKit stack but leaves libnghttp2 to the
  # host, since it sits on the AppImage excludelist.
  extraPkgs = pkgs: [ nghttp2.lib ];

  extraInstallCommands = ''
    install -Dm444 ${contents}/usr/share/applications/Unsloth.desktop \
      $out/share/applications/${pname}.desktop
    substituteInPlace $out/share/applications/${pname}.desktop \
      --replace-fail "Exec=unsloth-studio" "Exec=$out/bin/${pname}" \
      --replace-fail "Categories=" "Categories=Development;Science;"
    cp -r ${contents}/usr/share/icons $out/share/
  '';

  meta = {
    description = "Unsloth Studio desktop app for local LLM fine-tuning";
    homepage = "https://github.com/unslothai/unsloth";
    license = lib.licenses.agpl3Only;
    mainProgram = pname;
    platforms = [ "x86_64-linux" ];
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
  };
}

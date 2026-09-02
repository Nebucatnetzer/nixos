{
  lib,
  stdenv,
  fetchurl,
  autoPatchelfHook,
  dpkg,
  makeWrapper,
  dbus,
  expat,
  fontconfig,
  freetype,
  glib,
  libarchive,
  libGL,
  libice,
  libpng,
  libsm,
  libx11,
  libxau,
  libxcb,
  libxcb-image,
  libxcb-keysyms,
  libxcb-render-util,
  libxcb-util,
  libxcb-wm,
  libxdmcp,
  libxext,
  libxi,
  libxkbcommon,
  libxrender,
  sqlite,
  zlib,
  zstd,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "qudedup-extract-tool";
  version = "1.1.6.25140";

  src = fetchurl {
    url = "https://download.qnap.com/Storage/Utility/QNAPQuDedupExToolUbuntux64-${finalAttrs.version}.deb";
    hash = "sha256-AUxsT6QTSuFzMXuiRXpN1xkeayQtO2n+0nhcsIyp3xA=";
  };

  nativeBuildInputs = [
    autoPatchelfHook
    dpkg
    makeWrapper
  ];

  # Qt 5.12.4 ships inside the .deb; only its system-level dependencies are needed here.
  buildInputs = [
    dbus
    expat
    fontconfig
    freetype
    glib
    libGL
    libarchive
    libice
    libpng
    libsm
    libx11
    libxau
    libxcb
    libxcb-image
    libxcb-keysyms
    libxcb-render-util
    libxcb-util
    libxcb-wm
    libxdmcp
    libxext
    libxi
    libxkbcommon
    libxrender
    sqlite
    stdenv.cc.cc.lib
    zlib
    zstd
  ];

  unpackPhase = ''
    runHook preUnpack
    dpkg-deb -x $src .
    runHook postUnpack
  '';

  dontConfigure = true;
  dontBuild = true;

  # The bundled libraries resolve each other, so they must be on the runpath too.
  appendRunpaths = [ "${placeholder "out"}/lib" ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin $out/lib $out/libexec
    cp -r usr/local/bin/QNAP/QuDedupExTool $out/libexec/
    cp -r usr/local/lib/QNAP/QuDedupExTool/. $out/lib/

    # The vendored zstd (1.3.5) and zlib keep their generic SONAMEs, so they win the
    # flat namespace over the copies libarchive links against. zstd predates
    # ZSTD_minCLevel, which libarchive needs, so the app dies on first archive call.
    # Drop both and let every consumer share the nixpkgs versions.
    rm $out/lib/libzstd.so* $out/lib/libz.so*

    # Upstream launcher hardcodes LD_LIBRARY_PATH=/usr/local/lib/QNAP/QuDedupExTool
    rm $out/libexec/QuDedupExTool/QuDedupExTool.sh

    # QNAP ships these platform plugins without the Qt libraries they link against
    # (WaylandClient, EglFSDeviceIntegration, Quick/Qml/WebSockets), so they can
    # never load. The app is an xcb client; keep xcb, minimal and offscreen.
    rm -f $out/libexec/QuDedupExTool/platforms/libqwayland-*.so \
          $out/libexec/QuDedupExTool/platforms/libqeglfs.so \
          $out/libexec/QuDedupExTool/platforms/libqwebgl.so \
          $out/libexec/QuDedupExTool/platforms/libqlinuxfb.so

    makeWrapper $out/libexec/QuDedupExTool/QuDedupExTool $out/bin/${finalAttrs.pname} \
      --set QT_PLUGIN_PATH $out/libexec/QuDedupExTool/plugins \
      --set QT_QPA_PLATFORM_PLUGIN_PATH $out/libexec/QuDedupExTool/platforms \
      --set QT_QPA_PLATFORM xcb

    install -Dm644 usr/share/pixmaps/QuDedupExTool.png -t $out/share/pixmaps/

    mkdir -p $out/share/applications
    cat >$out/share/applications/${finalAttrs.pname}.desktop <<EOF
    [Desktop Entry]
    Type=Application
    Name=QuDedup Extract Tool
    Comment=Restore QNAP deduplicated .qdff backup files
    Exec=$out/bin/${finalAttrs.pname}
    Icon=$out/share/pixmaps/QuDedupExTool.png
    Categories=Utility;Archiving;
    EOF

    runHook postInstall
  '';

  meta = {
    description = "QNAP tool for restoring deduplicated .qdff backup files";
    homepage = "https://www.qnap.com/en/utilities/enterprise";
    # Proprietary QNAP binary redistributed from download.qnap.com; GUI only, no CLI mode.
    license = lib.licenses.unfree;
    mainProgram = finalAttrs.pname;
    platforms = [ "x86_64-linux" ];
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
  };
})

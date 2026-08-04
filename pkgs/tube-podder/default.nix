{
  lib,
  writers,
  yt-dlp,
}:
lib.meta.addMetaAttrs
  {
    description = "A bashpodder implementation in Python.";
    license = lib.licenses.gpl3Plus;
    mainProgram = "tube-podder";
    platforms = lib.platforms.linux;
  }
  (
    writers.writePython3Bin "tube-podder" {
      flakeIgnore = [ "E501" ];
      makeWrapperArgs = [
        "--prefix"
        "PATH"
        ":"
        (lib.makeBinPath [ yt-dlp ])
      ];
    } (builtins.readFile ./tube_podder.py)
  )

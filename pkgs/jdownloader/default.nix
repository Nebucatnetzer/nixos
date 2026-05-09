{
  lib,
  openjdk,
  ffmpeg,
  writeShellApplication,
}:
writeShellApplication {
  name = "jdownloader";
  runtimeInputs = [
    openjdk
    ffmpeg
  ];
  meta = {
    description = "Launch JDownloader 2 download manager";
    license = lib.licenses.gpl3Plus;
    mainProgram = "jdownloader";
    platforms = lib.platforms.linux;
  };
  text = ''
    java -jar "/home/andreas/applications/jd2/JDownloader.jar"
  '';
}

{
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
  text = ''
    java -jar "/home/andreas/applications/jd2/JDownloader.jar"
  '';
}

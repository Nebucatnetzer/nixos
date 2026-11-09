{
  lib,
  playwright-driver,
  python3Packages,
  writers,
}:
let
  browsers = playwright-driver.browsers.override {
    withChromiumHeadlessShell = false;
    withFirefox = false;
    withWebkit = false;
  };
in
lib.meta.addMetaAttrs
  {
    description = "Save every URL in a file into Zotero with the Zotero Connector";
    license = lib.licenses.gpl3Plus;
    mainProgram = "org-to-zotero";
    platforms = lib.platforms.linux;
  }
  (
    writers.writePython3Bin "org-to-zotero" {
      flakeIgnore = [ "E501" ];
      libraries = [ python3Packages.playwright ];
      makeWrapperArgs = [
        "--set"
        "PLAYWRIGHT_BROWSERS_PATH"
        "${browsers}"
        "--set"
        "PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS"
        "true"
      ];
    } (builtins.readFile ./org_to_zotero.py)
  )

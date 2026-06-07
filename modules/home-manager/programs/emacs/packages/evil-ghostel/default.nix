{
  writeText,
  melpaBuild,
  evil,
  ghostel,
}:
let
  pname = "evil-ghostel";
in
# https://github.com/dakra/ghostel (extensions/evil-ghostel)
# Source rev tracks the ghostel package so the two stay API-compatible.
melpaBuild {
  inherit pname;
  inherit (ghostel) version src;
  packageRequires = [
    evil
    ghostel
  ];
  recipe = writeText "recipe" ''
    (evil-ghostel :fetcher github :repo "dakra/ghostel" :files ("extensions/evil-ghostel/*.el"))
  '';
}

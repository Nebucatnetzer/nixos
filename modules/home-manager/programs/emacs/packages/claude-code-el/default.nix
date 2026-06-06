{
  fetchFromGitHub,
  inheritenv,
  melpaBuild,
}:
let
  pname = "claude-code";
in
# https://github.com/stevemolitor/claude-code.el
melpaBuild {
  inherit pname;
  version = "20260430";
  src = fetchFromGitHub {
    owner = "stevemolitor";
    repo = "${pname}.el";
    rev = "03199df8b3a1e9cd4857f0851f7a912ba524aff3";
    hash = "sha256-5QJrWIu4EgnHcOqMwlrs2JBBx7aI9OaSJswesr6Apfk=";
  };
  packageRequires = [ inheritenv ];
}

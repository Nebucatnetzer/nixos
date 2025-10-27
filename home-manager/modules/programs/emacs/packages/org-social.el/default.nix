{
  emojify,
  fetchFromGitHub,
  melpaBuild,
  org,
  request,
}:
let
  pname = "org-social";
  dependencies = [
    emojify
    org
    request
  ];
in
melpaBuild {
  inherit pname;
  version = "2.3.0";
  src = fetchFromGitHub {
    owner = "tanrax";
    repo = "${pname}.el";
    rev = "463f91bda64bede60ed0278239f6243615a097bc";
    hash = "sha256-/fXXFRJ5HtSDji6QmwSjJadz/k2rnBO6d4Fno+xmPVA=";
  };

  files = ''(:defaults "ui/*.el" "ui/**/*.el")'';

  propagatedUserEnvPkgs = dependencies;
  buildInputs = dependencies;
}

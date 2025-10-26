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
  version = "2.2.0";
  src = fetchFromGitHub {
    owner = "tanrax";
    repo = "${pname}.el";
    rev = "51dfaeb4c512d5b0bc14322554fdd4a62555b5fa";
    hash = "sha256-IfKnf0H3+HjbuaGdwUBWV/4oiUcL7z79cPLTzSDKZvk=";
  };

  files = ''(:defaults "ui/*.el" "ui/**/*.el")'';

  propagatedUserEnvPkgs = dependencies;
  buildInputs = dependencies;
}

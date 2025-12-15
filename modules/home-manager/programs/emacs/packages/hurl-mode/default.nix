{
  fetchFromGitHub,
  melpaBuild,
}:
let
  pname = "hurl-mode";
in
# https://github.com/JasZhe/hurl-mode
melpaBuild {
  inherit pname;
  version = "20251202";
  src = fetchFromGitHub {
    owner = "JasZhe";
    repo = "${pname}";
    rev = "2344f7c4b15192e01ffbff6d763beba76af47467";
    hash = "sha256-LNCypwmtzdBhia6FnOHi52TkhL/coKNZW8LtKqR9g64=";
  };
}

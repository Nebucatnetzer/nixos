{ config, inputs, ... }:
{
  age.secrets.resticKey = {
    file = "${inputs.self}/scrts/restic.key.age";
    mode = "600";
    owner = config.az-username;
    group = "users";
  };
}

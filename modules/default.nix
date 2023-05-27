{ custom }: { ... }:
{
  imports = [
    (import ./eog { inherit custom; })
    ./lockscreen
    (import ./makemkv { inherit custom; })
  ];
}

{ custom }: { ... }:
{
  imports = [
    (import ./eog { inherit custom; })
    (import ./makemkv { inherit custom; })
  ];
}

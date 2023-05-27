{ custom }: { ... }:
{
  imports = [
    (import ./eog { inherit custom; })
  ];
}

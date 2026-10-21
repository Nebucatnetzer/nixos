# Syncthing folders, keyed by label. path is relative to the host's dataRoot and
# defaults to the label. homePath is relative to the home directory instead.
# devices are hosts.nix keys.
{
  "01_inbox" = {
    id = "vhhhp-fgvts";
    devices = [
      "capricorn"
      "fenoglio"
      "gwyn"
      "phone"
      "red"
    ];
  };
  "02_projects" = {
    id = "dyzv4-c5i79";
    devices = [
      "capricorn"
      "fenoglio"
      "gwyn"
      "phone"
    ];
  };
  "03_notes" = {
    id = "b24il-hvijp";
    devices = [
      "capricorn"
      "fenoglio"
      "gwyn"
      "phone"
      "red"
    ];
  };
  "20_pictures" = {
    id = "xn6td-bdakn";
    devices = [
      "capricorn"
      "phone"
    ];
  };
  "30_keepass" = {
    id = "drhic-3jmuj";
    devices = [
      "capricorn"
      "gwyn"
      "phone"
    ];
  };
  "40_books" = {
    id = "gbsei-pfetk";
    devices = [
      "capricorn"
      "gwyn"
    ];
  };
  "98_zotero" = {
    id = "xd2p4-ur6jl";
    devices = [
      "capricorn"
      "gwyn"
      "red"
    ];
  };
  "99_archive" = {
    id = "eniad-xcj5j";
    devices = [
      "capricorn"
      "fenoglio"
      "gwyn"
    ];
  };
  phone-photos = {
    id = "rxcuy-k4fbv";
    homePath = "Downloads/inbox/photos";
    type = "receiveonly";
    devices = [
      "capricorn"
      "phone"
    ];
  };
  snapseed = {
    id = "x979j-lp6qy";
    homePath = "Downloads/inbox/photos";
    type = "receiveonly";
    devices = [
      "capricorn"
      "phone"
    ];
  };
}

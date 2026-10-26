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
  "40_reading" = {
    id = "40_reading";
    devices = [
      "capricorn"
      "phone"
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
  keepass = {
    id = "drhic-3jmuj";
    path = "98_applications/keepass";
    devices = [
      "capricorn"
      "gwyn"
      "phone"
    ];
  };
  org = {
    id = "org";
    path = "98_applications/org";
    devices = [
      "capricorn"
      "fenoglio"
      "gwyn"
      "red"
    ];
  };
  phone-photos = {
    id = "rxcuy-k4fbv";
    homePath = "Downloads/inbox/photos/fairphone/dcim";
    type = "receiveonly";
    devices = [
      "capricorn"
      "phone"
    ];
  };
  photos = {
    id = "xn6td-bdakn";
    path = "20_pictures/photos";
    devices = [
      "capricorn"
      "phone"
    ];
  };
  snapseed = {
    id = "x979j-lp6qy";
    homePath = "Downloads/inbox/photos/fairphone/snapseed";
    type = "receiveonly";
    devices = [
      "capricorn"
      "phone"
    ];
  };
  zotero = {
    id = "xd2p4-ur6jl";
    path = "98_applications/zotero";
    devices = [
      "capricorn"
      "gwyn"
      "red"
    ];
  };
}

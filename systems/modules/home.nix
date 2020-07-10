{ ... }:

{
  fileSystems = {
    "/mnt/archive" = {
      device = "odin:/volume1/archive";
      fsType = "nfs";
      options = ["x-systemd.automount" "noauto"];
    };
    "/mnt/torrents" = {
      device = "odin:/volume1/torrents";
      fsType = "nfs";
      options = ["x-systemd.automount" "noauto"];
    };
  };
}

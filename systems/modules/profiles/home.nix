{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.home;
in
{
  options = {
    profiles.home = {
      enable = mkEnableOption "Enable home profile";
    };
  };
  config = mkIf cfg.enable {
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
  };
}

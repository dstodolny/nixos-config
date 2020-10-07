{ ... }:

let
  hostname = "tyr";
  secrets = import ../secrets.nix;
  shared = import ../shared.nix;
in
{
  imports = [
    ./hardware/x220.nix
    ./modules/laptop.nix
    ./modules/base.nix
    ./modules/desktop.nix
  ];
  boot = {
    loader.grub = {
      enable = true;
      version = 2;
      device = "/dev/sda";
    };
    initrd.luks = {
      reusePassphrases = true;
      devices = {
        crypt-root = {
          device = "/dev/disk/by-uuid/e06d8d22-654f-41d2-9bc4-463848b719ab";
        };
      };
    };
  };
  networking = {
    hostName = hostname;
    extraHosts = shared.extraHosts;
  };
}

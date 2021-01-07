{ ... }:

let
  hostname = "tyr";
  secrets = import ../secrets.nix;
  shared = import ../shared.nix;
  hostsBlacklist = builtins.fetchurl {
    url = "https://raw.githubusercontent.com/StevenBlack/hosts/750faf4b129f3285cc659d02ce4560fdb7341e8e/alternates/fakenews-gambling-porn/hosts";
    sha256 = "0wn31kkavg0iy8yfpxmp8xmlnmjid8iv9gav0f6spr3dbl56xdsw";
  };
in
{
  imports = [
    ./hardware/x220.nix
    ./modules/laptop.nix
    ./modules/base.nix
    ./modules/desktop.nix
    ./modules/gaming.nix
    ./modules/tor.nix
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
    # extraHosts = (builtins.readFile hostsBlacklist) + shared.extraHosts;
  };
}

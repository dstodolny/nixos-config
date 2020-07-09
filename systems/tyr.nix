{ config, lib, ... }:
with lib;
let
  hostname = "tyr";
  secrets = import ../secrets.nix;
  shared = import ../shared.nix;
in
{
  imports = [
    ./hardware/x220.nix
    ./modules
    (import ../user)
  ];
  boot = {
    extraModulePackages = [ config.boot.kernelPackages.wireguard ];
    loader.grub = {
      enable = true;
      version = 2;
      device = "/dev/sda";
    };
    initrd.luks.devices = {
      crypt-lvm = {
        device = "/dev/disk/by-uuid/fbbd0ea1-9839-477b-a47d-0fbe5de5d270";
      };
    };
    cleanTmpDir = true;
  };
  profiles = {
    autologin.enable = true;
    desktop.enable = true;
    home.enable = true;
    laptop.enable = true;
    tor.enable = true;
  };
  time.timeZone = "Europe/London";
  networking = {
    hostName = hostname;
    extraHosts = shared.extraHosts;
    nat = {
      enable = true;
      externalInterface = "wlp2s0";
      internalInterfaces = [ "wg0" ];
    };
    firewall = {
      allowedUDPPorts = [ shared.ports.wireguard ];
      extraCommands = ''
        iptables -t nat -A POSTROUTING -s 10.206.94.0/24 -o wlp2s0 -j MASQUERADE
      '';
    };
    wireguard.interfaces = {
      wg0 = {
        ips = shared.wireguard.interfaces.tyr.ips;
        listenPort = shared.ports.wireguard;
        privateKey = secrets.wireguard.privateKeys.tyr;
        peers = [
          shared.wireguard.peers.njord
          shared.wireguard.peers.hel
        ];
      };
    };
  };
}

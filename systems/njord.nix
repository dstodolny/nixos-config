{ ... }:

let
  hostname = "njord";
  secrets = import ../secrets.nix;
  shared = import ../shared.nix;
in
{
  imports = [
    ./modules/base.nix
    ./modules/ssh.nix
  ];
  boot = {
    cleanTmpDir = true;
    loader = {
      grub.device = "/dev/vda";
      grub.enable = true;
      grub.version = 2;
    };
    kernel = {
      sysctl."vm.overcommit_memory" = "1";
    };
  };
  networking = {
    hostName = hostname;
    extraHosts = shared.extraHosts;
    firewall = {
      allowedUDPPorts = [ shared.ports.wireguard ];
      extraCommands = ''
        iptables -t nat -A POSTROUTING -s 10.206.94.0/24 -o ens3 -j MASQUERADE
      '';
    };
    nat = {
      enable = true;
      externalInterface = "ens3";
      internalInterfaces = [ "wg0" ];
    };
    wireguard.interfaces = {
      wg0 = {
        ips = shared.wireguard.interfaces.njord.ips;
        listenPort = shared.ports.wireguard;
        privateKey = secrets.wireguard.privateKeys.njord;
        peers = [
          shared.wireguard.peers.tyr
          shared.wireguard.peers.heimdall
          shared.wireguard.peers.hel
        ];
      };
    };
  };
}

{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.ssh;
  shared = import ../../shared.nix;
in
{
  options = {
    services.ssh = {
      enable = mkEnableOption "Enable ssh profile";
    };
  };
  config = mkIf cfg.enable {
    services.openssh = {
      enable = true;
      passwordAuthentication = false;
      permitRootLogin = "no";
      ports = [ shared.ports.sshd ];
    };
  };
}

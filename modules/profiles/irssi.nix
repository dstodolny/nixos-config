{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.irssi;
  secrets = import ../../secrets.nix;
in
{
  options = {
    profiles.irssi = {
      enable = mkOption {
        default = false;
        description = "Enable irssi profile and configuration";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    programs.irssi = {
      enable = true;
      extraConfig = ''
        servers = (
          {
            address = "localhost";
            chatnet = "Bitlbee";
            port = "6667";
            use_ssl = "no";
            ssl_verify = "no";
            autoconnect = "yes";
          }
        );
        chatnets = {
          Bitlbee = { type = "IRC"; autosendcmd = "/^msg -bitlbee &bitlbee identify ${secrets.bitlbee.password}; wait -bitlbee 2000"; };
        };
      '';
    };
  };
}

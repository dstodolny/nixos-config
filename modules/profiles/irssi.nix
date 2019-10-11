{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.irssi;
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
    };
  };
}

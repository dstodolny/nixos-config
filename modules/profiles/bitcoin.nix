{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.bitcoin;
in
{
  options = {
    profiles.bitcoin = {
      enable = mkOption {
        default = false;
        description = "Enable bitcoin profile";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      unstable.wasabiwallet
    ];
  };
}

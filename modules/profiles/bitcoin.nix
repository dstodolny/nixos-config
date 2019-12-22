{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.bitcoin;
in
{
  options = {
    profiles.bitcoin = {
      enable = mkEnableOption "Enable bitcoin profile";
    };
  };
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      unstable.wasabiwallet
    ];
  };
}

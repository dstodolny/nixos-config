{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.dev.c;
in
{
  options = {
    profiles.dev.c = {
      enable = mkEnableOption "Enable c development profile";
    };
  };
  config = mkIf cfg.enable {
    profiles.dev.enable = true;
    home.packages = with pkgs; [
      gcc
    ];
  };
}

{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.desktop;
in
{
  options = {
    profiles.desktop = {
      enable = mkEnableOption "Enable desktop profile";
      redshift = mkEnableOption "Enable redshift";
    };
  };
  config = mkIf cfg.enable {
    services = {
      redshift = {
        enable = cfg.redshift;
        latitude = "51.5094";
        longitude = "0.1365";
        brightness = {
          day = "1";
          night = "0.8";
        };
        temperature.night = 3000;
      };
      flameshot.enable = true;
    };
    home.packages = with pkgs; [
      firefox
      mpv
      xclip
    ];
  };
}

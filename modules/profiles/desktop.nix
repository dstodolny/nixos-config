{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.desktop;
in
{
  options = {
    profiles.desktop = {
      enable = mkOption {
        default = false;
        description = "Enable desktop profile";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    profiles = {
      gpg.enable = true;
      suckless.enable = true;
    };

    xsession.enable = true;
    xsession.windowManager.command = "exec ${pkgs.dwm}/bin/dwm";
    xsession.profileExtra = "slstatus &";
    services = {
      network-manager-applet.enable = true;
      redshift = {
        enable = true;
        latitude = "51.5094";
        longitude = "0.1365";
        brightness = {
          day = "1";
          night = "0.8";
        };
        temperature.night = 3000;
      };
    };
    programs = {
      firefox.enable = true;
    };
    home.packages = with pkgs; [
      gimp
      mpv
      sxiv
      xclip
    ];
  };
}

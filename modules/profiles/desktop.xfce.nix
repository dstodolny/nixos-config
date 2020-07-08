{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.desktop.xfce;
in
{
  options = {
    profiles.desktop.xfce = {
      enable = mkEnableOption "Enable xfce desktop profile";
    };
  };
  config = mkIf cfg.enable {
    profiles = {
      desktop.enable = true;
    };
    home = {
      file = {
        ".xinitrc" = {
          source = ../../assets/xinitrc;
        };
      };
    };
  };
}

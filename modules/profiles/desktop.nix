{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.desktop;
in
{
  options = {
    profiles.desktop = {
      enable = mkEnableOption "Enable desktop profile";
    };
  };
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      epdfview
      firefox
      thunderbird
      tor-browser-bundle-bin
      mpv
    ];
  };
}

{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.im;
in
{
  options = {
    profiles.im = {
      enable = mkEnableOption "Enable im profile";
    };
  };
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      tdesktop
    ];
  };
}

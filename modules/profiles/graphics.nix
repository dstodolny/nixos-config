{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.graphics;
in
{
  options = {
    profiles.graphics = {
      enable = mkEnableOption "Enable graphicselopment profile";
    };
  };
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      gimp
      sxiv
    ];
  };
}

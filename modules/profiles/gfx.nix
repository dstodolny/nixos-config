{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.gfx;
in
{
  options = {
    profiles.gfx = {
      enable = mkEnableOption "Enable gfx profile";
    };
  };
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      gimp
      sxiv
    ];
  };
}

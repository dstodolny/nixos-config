{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.dev.scheme;
in
{
  options = {
    profiles.dev.scheme = {
      enable = mkEnableOption "Enable scheme development profile";
    };
  };
  config = mkIf cfg.enable {
    profiles.dev.enable = true;
    home.packages = with pkgs; [
      mitscheme
    ];
  };
}

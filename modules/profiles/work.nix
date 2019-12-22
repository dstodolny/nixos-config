{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.work;
in
{
  options = {
    profiles.work = {
      enable = mkEnableOption "Enable work profile";
    };
  };
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      chromium
      slack
      unstable.postman
    ];
  };
}

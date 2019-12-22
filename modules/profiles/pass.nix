{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.pass;
in
{
  options = {
    profiles.pass = {
      enable = mkEnableOption "Enable pass";
    };
  };
  config = mkIf cfg.enable {
    profiles.gpg.enable = true;
    home.packages = with pkgs; [
      pass-otp
    ];
  };
}

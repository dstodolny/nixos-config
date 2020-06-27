{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.wechall;
  secrets = import ../../secrets.nix;
in
{
  options = {
    profiles.wechall = {
      enable = mkEnableOption "Enable wechall profile";
    };
  };
  config = mkIf cfg.enable {
    home = {
      sessionVariables = {
        WECHALLUSER = secrets.wechall.user;
        WECHALLTOKEN = secrets.wechall.token;
      };
    };
    programs.ssh = {
      enable = true;
      matchBlocks = {
        "labs.overthewire.org" = {
          host = "*.labs.overthewire.org";
          sendEnv = ["WECHALLTOKEN" "WECHALLUSER"];
        } ;
      };
    };
  };
}

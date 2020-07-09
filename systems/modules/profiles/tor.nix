{ config, lib, ... }:

with lib;
let
  cfg = config.profiles.tor;
in
{
  options = {
    profiles.tor = {
      enable = mkEnableOption "Enable tor profile";
    };
  };
  config = mkIf cfg.enable {
    services = {
      tor = {
        enable = true;
        client.enable = true;
      };
    };
  };
}

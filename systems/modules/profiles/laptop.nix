{ config, lib, ... }:

with lib;
let
  cfg = config.profiles.laptop;
in
{
  options = {
    profiles.laptop = {
      enable = mkEnableOption "Enable laptop profile";
    };
  };
  config = mkIf cfg.enable {
    services.acpid.enable = true;
    powerManagement.enable = true;
  };
}

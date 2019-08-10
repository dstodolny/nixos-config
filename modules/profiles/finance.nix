{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.finance;
in
{
  options = {
    profiles.finance = {
      enable = mkOption {
        default = false;
        description = "Enable finance profile";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    home.packages = with pkgs; [ ledger ];
  };
}

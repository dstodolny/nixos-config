{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.finance;
in
{
  options = {
    profiles.finance = {
      enable = mkEnableOption "Enable finance profile";
    };
  };
  config = mkIf cfg.enable {
    home.packages = with pkgs; [ ledger ];
  };
}

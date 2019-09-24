{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.sxhkd;
in
{
  options = {
    profiles.sxhkd = {
      enable = mkOption {
        default = false;
        description = "Enable sxhkd profile and configuration";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    services.sxhkd = {
      enable = true;
      keybindings = {
        "super + Return" = "st";
        "super + n" = "st -e newsboat";
        "super + p" = "dmenu_pass";
        "super + x" = "slock & xset dpms force off";
        "super + d" = "dmenu_run";
        "super + s" = "st -e htop";
        "super + c" = "st -e nvim ~/personal/gtd/inbox.md";
        "super + shift + w" = "tabbed -r 2 surf -e whatever";
      };
    };
  };
}

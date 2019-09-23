{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.zathura;
in
{
  options = {
    profiles.zathura = {
      enable = mkOption {
        default = false;
        description = "Enable zathura profile and configuration";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    programs.zathura = {
      enable = true;
      options = {
        statusbar-h-padding = 0;
        status-bar-v-padding = 0;
        page-padding = 1;
      };
      extraConfig = ''
        map u scroll half-up
        map d scroll half-down
        map D toggle_page_mode
        map r reload
        map R rotate
        map K zoom in
        map J zoom out
        map i recolor
        map p print
      '';
    };
  };
}

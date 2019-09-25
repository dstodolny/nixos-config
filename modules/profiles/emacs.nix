{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.emacs;
in
{
  options = {
    profiles.emacs = {
      enable = mkOption {
        default = false;
        description = "Enable emacs profile";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    services.emacs.enable = true;
    programs.emacs = {
      enable = true;
      extraPackages = epkgs: [
        epkgs.magit
        epkgs.expand-region
        epkgs.helm
        epkgs.flycheck
        # epkgs.gotham-theme
      ];
    };
  };
}

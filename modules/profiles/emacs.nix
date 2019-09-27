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
        epkgs.flycheck
        epkgs.neotree
        epkgs.rainbow-mode
        epkgs.projectile
        epkgs.ivy
        epkgs.swiper
        epkgs.counsel
        epkgs.tide
        epkgs.company
        epkgs.web-mode
        epkgs.haskell-mode
        epkgs.prettier-js
        epkgs.nix-mode
        epkgs.add-node-modules-path
        epkgs.flx
        epkgs.hindent
        epkgs.intero
        # epkgs.gotham-theme
      ];
    };
  };
}

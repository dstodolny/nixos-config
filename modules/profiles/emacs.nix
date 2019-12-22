{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.emacs;
in
{
  options = {
    profiles.emacs = {
      enable = mkEnableOption "Enable emacs profile";
    };
  };
  config = mkIf cfg.enable (mkMerge [
    {
      services.emacs.enable = !config.profiles.desktop.exwm.enable;
      programs.emacs = {
        enable = true;
        extraPackages = epkgs: with epkgs; [
           gruvbox-theme
        ];
      };
    }
    (mkIf config.profiles.desktop.exwm.enable {
      programs.emacs = {
        extraPackages = epkgs: with epkgs; [
          exwm
        ];
      };
    })
    (mkIf config.profiles.desktop.dwm.enable {
      programs.emacs = {
        extraPackages = epkgs: with epkgs; [
          magit
          expand-region
          flycheck
          graphql-mode
          neotree
          rainbow-mode
          projectile
          ivy
          swiper
          counsel
          tide
          company
          web-mode
          haskell-mode
          prettier-js
          nix-mode
          add-node-modules-path
          flx
          hindent
        ];
      };
    })
    (mkIf config.services.gpg-agent.enable {
      services.gpg-agent.extraConfig = ''
        #allow-emacs-pinentry
        #allow-loopback-pinentry
      '';
    })
  ]);
}

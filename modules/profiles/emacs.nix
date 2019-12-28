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
          company
          elfeed
          expand-region
          flycheck
          graphql-mode
          gruvbox-theme
          haskell-mode
          helm
          helm-pass
          helm-projectile
          helpful
          magit
          neotree
          nix-mode
          pdf-tools
          pinentry
          projectile
          rainbow-delimiters
          tide
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
          rainbow-mode
          ivy
          swiper
          counsel
          web-mode
          prettier-js
          add-node-modules-path
          flx
          hindent
        ];
      };
    })
    (mkIf config.services.gpg-agent.enable {
      services.gpg-agent.extraConfig = ''
        allow-emacs-pinentry
        allow-loopback-pinentry
      '';
    })
  ]);
}

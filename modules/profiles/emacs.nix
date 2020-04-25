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
          diminish
          esh-autosuggest
          expand-region
          flycheck
          helm
          helm-company
          helm-descbinds
          helm-exwm
          helm-pass
          helm-projectile
          helm-slime
          ledger-mode
          magit
          modus-operandi-theme
          modus-vivendi-theme
          nix-mode
          paredit
          pdf-tools
          pinentry
          projectile
          pulseaudio-control
          slime
          use-package
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
    (mkIf config.services.gpg-agent.enable {
      services.gpg-agent.extraConfig = ''
        allow-emacs-pinentry
        allow-loopback-pinentry
      '';
    })
  ]);
}

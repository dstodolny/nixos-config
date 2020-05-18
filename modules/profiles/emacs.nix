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
      nixpkgs.overlays = [
        (import (builtins.fetchTarball {
          url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
        }))
      ];
      services.emacs.enable = !config.profiles.desktop.exwm.enable;
      programs.emacs = {
        enable = true;
        package = pkgs.emacsGit;
        extraPackages = epkgs: with epkgs; [
          async
          diff-hl
          diminish
          dired-subtree
          diredfl
          esh-autosuggest
          expand-region
          flycheck
          ibuffer-vc
          icomplete-vertical
          ledger-mode
          magit
          modus-operandi-theme
          modus-vivendi-theme
          nix-mode
          orderless
          paredit
          pass
          password-store
          password-store-otp
          pdf-tools
          peep-dired
          pinentry
          prettier-js
          projectile
          pulseaudio-control
          rg
          slime
          tide
          trashed
          use-package
          wgrep
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

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
      services.emacs.enable = true;
      programs.emacs = {
        enable = true;
        package = pkgs.emacsGit;
        extraPackages = epkgs: with epkgs; [
          ace-window
          async
          diff-hl
          diminish
          dired-subtree
          diredfl
          esh-autosuggest
          expand-region
          flycheck
          goto-last-change
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
          prettier-js
          projectile
          pulseaudio-control
          restclient
          scratch
          sly
          tide
          trashed
          use-package
        ];
      };
    }
  ]);
}

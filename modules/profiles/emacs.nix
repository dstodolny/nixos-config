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
  config = mkIf cfg.enable (mkMerge [
    {
      home.file.".emacs.d".source = ../../assets/emacs.d;
      programs.emacs = {
        enable = true;
        extraPackages = epkgs: with epkgs; [
          bash-completion
          circe
          company
          csv-mode
          daemons
          desktop-environment
          elfeed
          esh-autosuggest
          expand-region
          exwm
          fish-completion
          flycheck
          geiser
          hackernews
          haskell-mode
          helm
          helm-company
          helm-descbinds
          helm-eww
          helm-flycheck
          helm-ls-git
          helm-pass
          helm-slime
          helm-system-packages
          helpful
          hl-todo
          iedit
          js2-mode
          ledger-mode
          lispy
          lispyville
          lua-mode
          magit
          magit-todos
          markdown-mode
          nhexl-mode
          nix-mode
          org
          org-bullets
          org-plus-contrib
          orgit
          pdf-tools
          pinentry
          prettier-js
          pulseaudio-control
          rainbow-delimiters
          rainbow-mode
          slime
          slime-company
          tide
          typescript-mode
          undo-tree
          web-mode
          ztree
        ];
      };
    }
    (mkIf config.services.gpg-agent.enable {
      services.gpg-agent.extraConfig = ''
        pinentry-program "${pkgs.pinentry_emacs}/bin/pinentry-emacs"
        allow-emacs-pinentry
        allow-loopback-pinentry
      '';
    })
  ]);
}

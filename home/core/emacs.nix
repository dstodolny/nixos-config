{ pkgs, emacs, ... }:

{
  services.emacs.enable = true;
  home.packages = with pkgs; [
    poppler_utils
  ];
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

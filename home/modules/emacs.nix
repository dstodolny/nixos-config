{ pkgs, ... }:

{
  services.emacs.enable = true;
  home.packages = with pkgs; [
    poppler_utils
  ];
  programs.emacs = {
    enable = true;
    package = pkgs.emacsGit;
    extraPackages = epkgs: with epkgs; [
      async
      diff-hl
      diminish
      dired-single
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
      paredit
      pass
      password-store
      password-store-otp
      pdf-tools
      prettier-js
      rg
      scratch
      sly
      tide
      trashed
      use-package
      wgrep
    ];
  };
}

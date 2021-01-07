{ pkgs, ... }:

{
  services.emacs.enable = true;
  programs.emacs = {
    enable = true;
    # package = pkgs.emacsGit;
    extraPackages = epkgs: with epkgs; [
      add-node-modules-path
      diff-hl
      diminish
      direnv
      elfeed
      exec-path-from-shell
      expand-region
      flycheck
      hl-todo
      ibuffer-vc
      icomplete-vertical
      ledger-mode
      lsp-mode
      magit
      modus-operandi-theme
      modus-vivendi-theme
      nix-mode
      orderless
      projectile
      rainbow-mode
      rg
      scratch
      use-package
      wgrep
    ];
  };
}

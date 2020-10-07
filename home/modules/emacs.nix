{ pkgs, ... }:

{
  services.emacs.enable = true;
  programs.emacs = {
    enable = true;
    package = pkgs.emacsGit;
    extraPackages = epkgs: with epkgs; [
      add-node-modules-path
      diff-hl
      direnv
      exec-path-from-shell
      expand-region
      flycheck
      haskell-mode
      hl-todo
      ibuffer-vc
      icomplete-vertical
      lsp-mode
      magit
      modus-operandi-theme
      modus-vivendi-theme
      nix-mode
      projectile
      rainbow-mode
      rg
      scratch
      use-package
      wgrep
    ];
  };
}

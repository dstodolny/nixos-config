{ pkgs, ... }:

{
  imports = [
    ./base.nix
  ];

  profiles = {
    bitcoin.enable = true;
    dev = {
      haskell.enable = true;
      javascript.enable = true;
      python.enable = true;
    };
    desktop.dwm.enable = true;
    emacs.enable = true;
    finance.enable = true;
    latex.enable = true;
    work.enable = true;
    zsh.enable = true;
  };
}

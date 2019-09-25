{ pkgs, ... }:

{
  imports = [
    ./base.nix
  ];

  profiles = {
    dev = {
      haskell.enable = true;
      javascript.enable = true;
      python.enable = true;
    };
    emacs.enable = true;
    finance.enable = true;
    laptop.enable = true;
    latex.enable = true;
    newsboat.enable = true;
    sxhkd.enable = true;
    work.enable = true;
    zathura.enable = true;
    zsh.enable = true;
  };
}

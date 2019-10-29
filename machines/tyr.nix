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
    emacs.enable = true;
    finance.enable = true;
    irssi.enable = true;
    laptop.enable = true;
    newsboat.enable = true;
    sxhkd.enable = true;
    zathura.enable = true;
    zsh.enable = true;
  };
}

{ pkgs, ... }:

{
  imports = [
    ./base.nix
  ];

  profiles = {
    laptop.enable = true;
    dev = {
      haskell.enable = true;
      javascript.enable = true;
      python.enable = true;
    };
    work.enable = true;
    finance.enable = true;
    latex.enable = true;
    newsboat.enable = true;
    sxhkd.enable = true;
    zsh.enable = true;
    zathura.enable = true;
  };

  home = {
    file = {
      "bin".source = ../assets/bin;
    };
  };
}

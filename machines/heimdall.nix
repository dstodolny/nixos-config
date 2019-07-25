{ pkgs, ... }:

{
  imports = [
    ./base.nix
  ];

  profiles.laptop.enable = true;
  profiles.dev = {
    haskell.enable = true;
    javascript.enable = true;
  };
  profiles.work.enable = true;

  home = {
    file = {
      "bin".source = ../assets/bin;
      ".emacs.d".source = ../assets/emacs.d;
    };
  };
}

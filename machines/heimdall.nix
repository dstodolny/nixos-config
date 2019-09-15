{ pkgs, ... }:

{
  imports = [
    ./base.nix
  ];

  profiles.laptop.enable = true;
  profiles.dev = {
    haskell.enable = true;
    javascript.enable = true;
    python.enable = true;
  };
  profiles.work.enable = true;
  profiles.finance.enable = true;
  profiles.latex.enable = true;

  home = {
    file = {
      "bin".source = ../assets/bin;
    };
  };
}

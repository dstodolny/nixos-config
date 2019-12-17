{ pkgs, ... }:

{
  imports = [
    ./base.nix
  ];

  profiles = {
    emacs.enable = true;
    laptop.enable = true;
    desktop.redshift = false;
    sxhkd.enable = false;
  };
}

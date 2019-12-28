{ config, pkgs, ... }:

let
  unstableTarball = fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz;
in
{
  imports = [ ../modules/module-list.nix ];
  programs.home-manager.enable = true;
  nixpkgs.config = {
    packageOverrides = with pkgs; {
      unstable = import unstableTarball {
        config = config.nixpkgs.config;
      };
    };
  };
  home = {
    file = {
      ".profile".source = ../assets/profile;
    };
    packages = with pkgs; [
      wget
      poppler
    ];
  };
}

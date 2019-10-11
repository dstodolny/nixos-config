{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.work;
  unstableTarball = fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz;
in
{
  options = {
    profiles.work = {
      enable = mkOption {
        default = false;
        description = "Enable work profile and configuration";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    nixpkgs.config = {
      allowUnfree = true;
      packageOverrides = pkgs: {
        unstable = import unstableTarball {
          config = config.nixpkgs.config;
        };
      };
    };
    home.packages = with pkgs; [
      chromium
      slack
      unstable.postman
    ];
  };
}

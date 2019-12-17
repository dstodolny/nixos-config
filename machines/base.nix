{ config, pkgs, ... }:

let
  unstableTarball = fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz;
  pkgs-19-03Zip = import(pkgs.fetchzip {
    url = "https://github.com/NixOS/nixpkgs/archive/19.03.zip";
    sha256 = "0q2m2qhyga9yq29yz90ywgjbn9hdahs7i8wwlq7b55rdbyiwa5dy";
  }) {};
in
{
  imports = [ ../modules/module-list.nix ];
  programs.home-manager.enable = true;
  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = with pkgs; {
      unstable = import unstableTarball {
        config = config.nixpkgs.config;
      };
      pkgs-19-03 = pkgs-19-03Zip;
    };
  };
  home = {
    file = {
      "bin".source = ../assets/bin;
      ".profile".source = ../assets/profile;
    };
    packages = with pkgs; [
      fzf
      tmux
      htop
      killall
      lf
      openssl
      pandoc
      pass-otp
      ripgrep
      urlscan
      w3m
      wget
      youtube-dl
    ];
  };
}

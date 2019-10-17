{ pkgs ? import <nixpkgs> {} }:

let
  # unstableTarball = fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz;
  # pkgs-19-03Zip = import(pkgs.fetchzip {
  #   url = "https://github.com/NixOS/nixpkgs/archive/19.03.zip";
  #   sha256 = "0q2m2qhyga9yq29yz90ywgjbn9hdahs7i8wwlq7b55rdbyiwa5dy";
  # }) {};
in
{

  # unstable = import unstableTarball {
  #   config = config.nixpkgs.config;
  # };
  # pkgs-19-03 = pkgs-19-03Zip;
  # my = pkgs.recurseIntoAttrs {
  #   electrum = pkgs.qt5.callPackage ./electrum { };
  # };
  # python3Packages = pkgs.python3Packages.override (oldAttrs: {
  #   overrides = self: super: {
  #     ckcc-protocol = pkgs.callPackage ./ckcc-protocol { };
  #   };
  # });
}

{ pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;
  home.packages = with pkgs; [
    libreoffice
    postman
    slack
    zoom-us
  ];
}

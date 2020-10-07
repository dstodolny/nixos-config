{ pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;
  home.packages = with pkgs; [
    chromium
    libreoffice
    postman
    slack
    zoom-us
  ];
}

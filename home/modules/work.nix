{ pkgs, ... }:

{
  home.packages = with pkgs; [
    libreoffice
    postman
    slack
    zoom-us
    vs
  ];
}

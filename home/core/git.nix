{ config, lib, ... }:

{
  programs.git = {
    enable = true;
    userName = "Dominik Stodolny";
    userEmail = "dominik@stodolny.org";
    signing = {
      key = "0D5591D3B4BB7818";
      signByDefault = true;
    };
    aliases = {
      ci = "commit --signoff";
      co = "checkout";
      st = "status";
    };
    extraConfig = {
      pull = {
        rebase = true;
      };
      push = {
        default = "current";
        recurseSubmodules = "check";
      };
      rebase = {
        autosquash = true;
      };
    };
  };
}

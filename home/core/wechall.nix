{ config, lib, pkgs, ... }:

let
  secrets = import ../../secrets.nix;
in
{
  home = {
    sessionVariables = {
      WECHALLUSER = secrets.wechall.user;
      WECHALLTOKEN = secrets.wechall.token;
    };
  };
  programs.ssh = {
    enable = true;
    matchBlocks = {
      "labs.overthewire.org" = {
        host = "*.labs.overthewire.org";
        sendEnv = ["WECHALLTOKEN" "WECHALLUSER"];
      } ;
    };
  };
}

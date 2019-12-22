{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.git;
in
{
  options = {
    profiles.git = {
      enable = mkOption {
        default = true;
        description = "Enable git profile";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    programs.git = {
      enable = true;
      userName = "Dominik Stodolny";
      userEmail = "dominik@stodolny.org";
      signing = mkIf config.profiles.gpg.enable {
        key = "0D5591D3B4BB7818";
        signByDefault = true;
      };
      aliases = {
        b = "branch --color -v";
        br = "branch";
        ci = "commit --signoff";
        co = "checkout";
        ca = "commit --amend";
        unstage = "reset HEAD";
        r = "remote -v";
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
  };
}

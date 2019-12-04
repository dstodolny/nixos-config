{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.git;
in
{
  options = {
    profiles.git = {
      enable = mkOption {
        default = false;
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
      signing = {
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
        w = "status -sb";
      };
      extraConfig = {
        core = {
          editor = "${pkgs.emacs}/bin/emacsclient -t";
        };
        forge = {
          remote = "upstream";
        };
        color = {
          status      = "auto";
          diff        = "auto";
          branch      = "auto";
          interactive = "auto";
          ui          = "auto";
          sh          = "auto";
        };
        "color \"branch\"" = {
          current = "cyan reverse";
          local = "cyan";
          remote = "green";
        };
        "color \"diff\"" = {
          current = "white reverse";
          frag = "magenta reverse";
          old = "red";
          new = "green";
        };
        "color \"status\"" = {
          added = "green";
          changed = "yellow";
          untracked = "red";
        };
        hub = {
          protocol = true;
        };
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
        advice = {
          statusHints = false;
          pushNonFastForward = false;
        };
        "diff \"gpg\"" = {
          binary = true;
          textconv = "gpg -d --quiet --yes --compress-algo=none --no-encrypt-to";
        };
      };
      ignores = [
        "TAGS"
      ];
    };
  };
}

{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.dev.javascript;
  secrets = import ../../secrets.nix;
in
{
  options = {
    profiles.dev.javascript = {
      enable = mkOption {
        default = false;
        description = "Enable javascript development profile";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    profiles.dev.enable = true;
    home.file.".npmrc".text = ''
      prefix = ~/.local/npm
      //registry.npmjs.org/:_authToken=${secrets.npmjs.tokens.atg}
    '';
    home.sessionVariables.PATH = "/home/dnixty/.local/npm/bin:$PATH";
    home.packages = with pkgs; [
      nodejs
    ];
  };
}

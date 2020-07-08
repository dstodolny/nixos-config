{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.dev.javascript;
  secrets = import ../../secrets.nix;
in
{
  options = {
    profiles.dev.javascript = {
      enable = mkEnableOption "Enable javascript development profile";
    };
  };
  config = mkIf cfg.enable {
    profiles.dev.enable = true;
    home = {
      file.".npmrc".text = ''
        prefix = ~/.local/npm
      '';
      sessionVariables.PATH = "/home/dnixty/.local/npm/bin:$PATH";
      packages = with pkgs; [
        nodejs-10_x
      ];
    };
  };
}

{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.latex;
in
  {
    options = {
      profiles.latex = {
        enable = mkOption {
          default = false;
          description = "Enable latex profile and configuration";
          type = types.bool;
        };
      };
    };
    config = mkIf cfg.enable {
      home.packages = with pkgs; [
        texlive.combined.scheme-full
      ];
    };
}

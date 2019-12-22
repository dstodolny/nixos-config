{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.latex;
in
  {
    options = {
      profiles.latex = {
        enable = mkEnableOption "Enable latex profile";
      };
    };
    config = mkIf cfg.enable {
      home.packages = with pkgs; [
        texlive.combined.scheme-full
      ];
    };
}

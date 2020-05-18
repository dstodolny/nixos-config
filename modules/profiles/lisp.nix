{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.dev.lisp;
in
{
  options = {
    profiles.dev.lisp = {
      enable = mkEnableOption "Enable lisp development profile";
    };
  };
  config = mkIf cfg.enable {
    profiles.dev.enable = true;
    home = {
      file = {
        ".sbclrc".source = ../../assets/sbclrc;
      };
      packages = with pkgs; [
        sbcl
        clisp
        lispPackages.quicklisp
      ];
    };
  };
}

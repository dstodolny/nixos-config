{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.desktop.exwm;
in
{
  options = {
    profiles.desktop.exwm = {
      enable = mkEnableOption "Enable exwm desktop profile";
    };
  };
  config = mkIf cfg.enable {
    profiles = {
      desktop.enable = true;
      emacs.enable = true;
      gpg.pinentry = "${pkgs.pinentry_emacs}/bin/pinentry-emacs";
    };
    home.file = {
      ".xsession" = {
        source = ../../assets/xsession;
        executable = true;
      };
      ".conkyrc".source = ../../assets/conkyrc;
    };
    home.packages = with pkgs; [
      pinentry_emacs
      conky
      dzen2
    ];
  };
}

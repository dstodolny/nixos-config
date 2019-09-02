{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.desktop;
in
{
  options = {
    profiles.desktop = {
      enable = mkOption {
        default = false;
        description = "Enable desktop profile";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    profiles.gpg.enable = true;

    xsession.enable = true;
    xsession.windowManager.command = "{pkgs.dwm}/bin/dwm";
    home.file = {
      ".xsession" = {
        source = ../../assets/xsession;
        executable = true;
      };
      ".xinitrc".source = ../../assets/xinitrc;
      ".xprofile".source = ../../assets/xprofile;
    };
    services = {
      network-manager-applet.enable = true;
      redshift = {
        enable = true;
        latitude = "51.5094";
        longitude = "0.1365";
        brightness = {
          day = "1";
          night = "0.8";
        };
        temperature.night = 3000;
      };
    };
    programs = {
      firefox.enable = true;
    };
    nixpkgs.config.packageOverrides = pkgs: {
      dwm = pkgs.dwm.override {
        patches = [
           ../../assets/dwm/dwm-theme.diff
           ../../assets/dwm/dwm-modkey.diff
        ];
      };
      st = pkgs.st.override {
        patches = [
          ../../assets/st/st-scrollback-0.8.2.diff
          ../../assets/st/st-scrollback-mouse-0.8.2.diff
          ../../assets/st/st-bold-is-not-bright-20190127-3be4cf1.diff
          ../../assets/st/st-visual.diff
          ../../assets/st/st-keys.diff
        ];
      };
    };
    home.packages = with pkgs; [
      dwm
      dmenu
      st
      pass-otp
      mpv
    ];
  };
}

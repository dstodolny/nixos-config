{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.desktop;
in
{
  options = {
    profiles.desktop = {
      enable = mkEnableOption "Enable desktop profile";
    };
  };
  config = mkIf cfg.enable {
    console.useXkbConfig = true;
    environment.systemPackages = with pkgs; [
      xss-lock
    ];
    fonts = {
      enableDefaultFonts = true;
      enableFontDir = true;
      enableGhostscriptFonts = true;
      fonts = with pkgs; [ hack-font ];
    };
    hardware = {
      bluetooth.enable = true;
      pulseaudio = {
        enable = true;
        package = pkgs.pulseaudioFull;
      };
    };
    networking.networkmanager.enable = true;
    programs.slock.enable = true;
    services = {
      blueman.enable = true;
      picom = {
        enable = true;
        fade = true;
        fadeDelta = 4;
        inactiveOpacity = "0.9";
        shadow = true;
      };
      printing = {
        enable = true;
        drivers = [ pkgs.hplip ];
      };
      xserver = {
        enable = true;
        xkbOptions = "ctrl:swapcaps";
        desktopManager.xfce.enable = true;
        displayManager.startx.enable = true;
      };
    };
    sound = {
      enable = true;
      mediaKeys.enable = true;
    };
  };
}

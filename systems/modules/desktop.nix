{ pkgs, lib, ... }:

{
  console.useXkbConfig = true;
  fonts = {
    fonts = with pkgs; [ hack-font ];
  };
  hardware = {
    bluetooth.enable = true;
    pulseaudio.enable = true;
  };
  services = {
    blueman.enable = true;
    gvfs = {
      enable = true;
      package = lib.mkForce pkgs.gnome3.gvfs;
    };
    printing = {
      enable = true;
      drivers = [ pkgs.hplip ];
    };
    xserver = {
      enable = true;
      xkbOptions = "ctrl:swapcaps";
      displayManager.defaultSession = "xfce";
      desktopManager = {
        xterm.enable = false;
        xfce.enable = true;
      };
    };
  };
  sound = {
    enable = true;
    mediaKeys.enable = true;
  };
}

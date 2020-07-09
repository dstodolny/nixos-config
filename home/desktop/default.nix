{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      epdfview
      firefox
      gimp
      imagemagick
      mpv
      sxiv
      tdesktop
      thunderbird
      tor-browser-bundle-bin
      wasabiwallet
    ];
    file = {
      ".xinitrc" = {
        source = ../assets/xinitrc;
      };
    };
  };
}

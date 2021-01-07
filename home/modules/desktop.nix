{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      megasync
      firefox
      lightlocker
      mate.atril
      mpv
      peek
      sxiv
      tdesktop
      thunderbird
      wasabiwallet
      xdotool
      stellarium
    ];
    file = {
      ".xprofile".text = ''
          export EDITOR="emacs"
          export PATH="~/.local/bin:$PATH"
        . "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
      '';
    };
  };
  xdg = {
    enable = true;
    userDirs = {
      enable = true;
      desktop = "\$HOME/Desktop";
      documents = "\$HOME/documents";
      download = "\$HOME/tmp";
      pictures = "\$HOME/pictures";
      music = "\$HOME/music";
      videos = "\$HOME/videos";
      templates = "\$HOME/documents";
      publicShare = "\$HOME/pub";
    };
  };
}

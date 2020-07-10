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
      wasabiwallet
    ];
    file = {
      ".xinitrc".text = ''
         xss-lock slock &
         gpg-connect-agent updatestartuptty /bye
         exec startxfce4
      '';
    };
  };
  xdg = {
    enable = true;
    userDirs = {
      enable = true;
      desktop = "$HOME/desktop";
      documents = "$HOME/desktop/documents";
      download = "$HOME/desktop/downloads";
      music = "$HOME/desktop/music";
      pictures = "$HOME/desktop/pictures";
      publicShare = "$HOME/desktop/sites";
      templates = "$HOME/desktop/documents/templates";
      videos = "$HOME/desktop/videos";
    };
  };
}

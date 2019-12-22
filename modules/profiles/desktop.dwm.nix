{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.desktop.dwm;
  secrets = import ../../secrets.nix;
in
{
  options = {
    profiles.desktop.dwm = {
      enable = mkEnableOption "Enable dwm desktop profile";
    };
  };
  config = mkIf cfg.enable {
    profiles = {
      desktop.enable = true;
      zsh.enable = true;
    };
    xsession = {
      enable = true;
      windowManager.command = "exec ${pkgs.dwm}/bin/dwm";
      profileExtra = "slstatus &";
    };
    nixpkgs.config.packageOverrides = pkgs: {
      dwm = pkgs.dwm.override {
        patches = [
          ../../assets/patches/dwm/dwm-custom-6.2.diff
        ];
      };
      dmenu = pkgs.dmenu.override {
        patches = [
          ../../assets/patches/dmenu/dmenu-custom-4.9.diff
        ];
      };
      st = pkgs.st.override {
        patches = [
          ../../assets/patches/st/st-custom-0.8.2.diff
        ];
      };
      surf = pkgs.surf.override {
        patches = [
          ../../assets/patches/surf/surf-custom-2.0.diff
        ];
      };
      tabbed = pkgs.tabbed.override {
        patches = [
          ../../assets/patches/tabbed/tabbed-custom.diff
        ];
      };
    };
    programs = {
      irssi = {
        enable = true;
        extraConfig = ''
          servers = (
            {
              address = "localhost";
              chatnet = "Bitlbee";
              port = "6667";
              use_ssl = "no";
              ssl_verify = "no";
              autoconnect = "yes";
            }
          );
          chatnets = {
            Bitlbee = { type = "IRC"; autosendcmd = "/^msg -bitlbee &bitlbee identify ${secrets.bitlbee.password}; wait -bitlbee 2000"; };
          };
        '';
      };
      newsboat = {
        enable = true;
        autoReload = true;
        urls = secrets.newsboat.urls;
        extraConfig = ''
          external-url-viewer "urlscan -dc -r 'linkhandler {}'"
          save-path "~/review"
          bind-key j down
          bind-key k up
          bind-key j next articlelist
          bind-key k prev articlelist
          bind-key J next-feed articlelist
          bind-key K prev-feed articlelist
          bind-key G end
          bind-key g home
          bind-key d pagedown
          bind-key u pageup
          bind-key l open
          bind-key h quit
          bind-key a toggle-article-read
          bind-key n next-unread
          bind-key N prev-unread
          bind-key D pb-download
          bind-key U show-urls
          bind-key x pb-delete

          color listnormal blue default
          color listfocus black yellow standout bold
          color listnormal_unread white default
          color listfocus_unread yellow default bold
          color info white black bold
          color article cyan default

          browser linkhandler
          macro , open-in-browser
          macro t set browser "tsp youtube-dl --add-metadata -ic"; open-in-browser ; set browser linkhandler
          macro a set browser "tsp youtube-dl --add-metadata -xic -f bestaudio/best"; open-in-browser ; set browser linkhandler
          macro v set browser "setsid nohup mpv"; open-in-browser ; set browser linkhandler
          macro w set browser "w3m"; open-in-browser ; set browser linkhandler
          macro p set browser "dmenuhandler"; open-in-browser ; set browser linkhandler
          macro c set browser "xsel -b <<<" ; open-in-browser ; set browser linkhandler
        '';
      };
      zathura = {
        enable = true;
        options = {
          statusbar-h-padding = 0;
          statusbar-v-padding = 0;
          page-padding = 1;
        };
        extraConfig = ''
          map u scroll half-up
          map d scroll half-down
          map D toggle_page_mode
          map r reload
          map R rotate
          map K zoom in
          map J zoom out
          map i recolor
          map p print
        '';
      };
    };
    services.sxhkd = {
      enable = true;
      keybindings = {
        "super + Return" = "st";
        "super + n" = "st -e newsboat";
        "super + p" = "passmenu";
        "super + d" = "dmenu_run";
        "super + e" = "emacsclient -c";
        "super + s" = "st -e htop";
        "super + a" = "st -e irssi";
        "super + c" = "st -e nvim ~/personal/gtd/inbox.md";
        "super + shift + e" = "systemctl --user restart emacs";
        "super + shift + w" = "tabbed -r 2 surf -e whatever";
        "super + shift + x" = "slock & xset dpms force off";
        "Print" = "scrot -fs -e";
      };
    };
    home.packages = with pkgs; [
      (slstatus.override { conf = builtins.readFile (../../assets/patches/slstatus/config.def.h); })
      tabbed
      dmenu
      dwm
      st
      surf
    ];
  };
}

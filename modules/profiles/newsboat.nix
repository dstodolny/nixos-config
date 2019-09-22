{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.newsboat;
  secrets = import ../../secrets.nix;
in
{
  options = {
    profiles.newsboat = {
      enable = mkOption {
        default = false;
        description = "Enable newsboat profile and configuration";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    programs.newsboat = {
      enable = true;
      autoReload = true;
      urls = secrets.newsboat.urls;
      extraConfig = ''
        external-url-viewer "urlscan -dc -r 'linkhandler {}'"
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

        color listnormal white default
        color listfocus black yellow standout bold
        color listnormal_unread blue default
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
  };
}

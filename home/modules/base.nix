{ pkgs, ... }:

{
  home = {
    stateVersion = "20.03";
    packages = with pkgs; [
      bc
      cmus
      ffmpeg
      killall
      ledger
      niv
      pinentry
      ripgrep
      unzip
      wget
      youtube-dl
    ];
    file = {
      ".local/bin" = {
        source = ../assets/bin;
        recursive = true;
      };
    };
  };
  programs = {
    ssh = {
      enable = true;
      matchBlocks = {
        "hel" = {
          hostname = "hel";
          user = "dnixty";
        };
        "njord.lan" = {
          hostname = "njord.lan";
          user = "dnixty";
        };
        "heimdall.lan" = {
          hostname = "heimdall.lan";
          user = "root";
        };
        "thor.lan" = {
          hostname = "thor.lan";
          user = "dnixty";
        };
      };
    };
    tmux = {
      enable = true;
      baseIndex = 1;
      clock24 = true;
      escapeTime = 0;
      historyLimit = 50000;
      keyMode = "emacs";
      shortcut = "a";
      terminal = "tmux-256color";
      newSession = true;
      extraConfig = ''
        set -g renumber-windows on
        set -gw alternate-screen off
        set -gw automatic-rename on
        set -gw monitor-activity on
        set -g mouse on
        set -g status-interval 1
        set -g status-left ""
        set -g status-left-length 40
        set -g status-right '%H:%M:%S'
        set -g status-right-length 100
        set -g status-style fg=default,bg=black
        set -gw pane-active-border-style fg=brightwhite
        set -gw pane-border-format ' #W '
        set -gw pane-border-status bottom
        set -gw pane-active-border-style fg=brightwhite
        set -gw window-status-activity-style fg=brightwhite
        set -gw window-status-bell-style fg=brightcyan
        set -gw window-status-current-format '#W'
        set -gw window-status-current-style fg=green,bg=default,bright
        set -gw window-status-format '#W'
        set -gw window-status-separator '  '
        set -gw window-status-style fg=default,bg=default
        bind -r C-a send-prefix
        bind c new-window -c "#{pane_current_path}"
        bind '"' split-window -c "#{pane_current_path}"
        bind % split-window -h -c "#{pane_current_path}"
      '';
    };
    gpg.enable = true;
    password-store = {
      enable = true;
      package = pkgs.pass.withExtensions (exts: [ exts.pass-otp ]);
      settings = { PASSWORD_STORE_KEY = "dominik@stodolny.org"; };
    };
    bash.shellAliases = {
      x = "sxiv -ator \"\${1:-.}\"";
      t = "tmux new -DAs 0";
      yt = "youtube-dl --add-metadata -i";
      yta = "yt -x -f bestaudio/best";
    };
  };
  services = {
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
      defaultCacheTtl = 86400;
      maxCacheTtl = 86400;
      defaultCacheTtlSsh = 86400;
      maxCacheTtlSsh = 86400;
    };
    password-store-sync.enable = true;
  };
}

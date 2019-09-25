{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.zsh;
in
{
  options = {
    profiles.zsh = {
      enable = mkOption {
        default = false;
        description = "Enable zsh profile";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    programs.fzf.enableZshIntegration = true;
    programs.zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;
      autocd = true;
      dotDir = ".config/zsh";
      defaultKeymap = "emacs";
      history = {
        ignoreDups = true;
        path = "personal/history/zsh";
      };
      initExtra = ''
        autoload -Uz vcs_info
        precmd_vcs_info() { vcs_info }
        precmd_functions+=( precmd_vcs_info )
        setopt prompt_subst
        zstyle ':vcs_info:git:*' formats '/@%b'
        PROMPT='%B(%F{cyan}%n%f@%F{cyan}%m%f%)[%F{green}%~%f'\$vcs_info_msg_0_']%b
        %(!.#.>) '
        RPROMPT=""
        if command -v fzf-share >/dev/null; then
          source "$(fzf-share)/key-bindings.zsh"
        fi

        # Use lf to switch directories and bind it to ctrl-o
        lfcd () {
          tmp="$(mktemp)"
          lf -last-dir-path="$tmp" "$@"
          if [ -f "$tmp" ]; then
            dir="$(cat "$tmp")"
            rm -f "$tmp"
            [ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
          fi
        }
        bindkey -s '^o' 'lfcd\n'

        source ${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
      '';
      initExtraBeforeCompInit = ''
        zstyle ':completion:*' menu select
        zmodload zsh/complist
      '';
      sessionVariables = {
        EDITOR = "nvim";
        TERMINAL = "st";
        BROWSER = "surf";
        READER = "zathura";
        FILE = "lf";
      };
      shellAliases = {
        e = "emacsclient";
        SS = "sudo systemctl";
        f = "$FILE";
        g = "git";
        v = "$EDITOR";
        ls = "ls -hN --color=auto --group-directories-first";
        diff = "diff --color=auto";
      };
    };
  };
}

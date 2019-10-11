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
        EDITOR = "emacsclient";
        TERMINAL = "st";
        BROWSER = "surf";
        READER = "zathura";
        FILE = "lf";
        LESS_TERMCAP_mb="$(tput bold; tput setaf 2)"; # green
        LESS_TERMCAP_md="$(tput bold; tput setaf 6)"; # cyan
        LESS_TERMCAP_me="$(tput sgr0)";
        LESS_TERMCAP_so="$(tput bold; tput setaf 3; tput setab 4)"; # yellow on blue
        LESS_TERMCAP_se="$(tput rmso; tput sgr0)";
        LESS_TERMCAP_us="$(tput smul; tput bold; tput setaf 7)"; # white
        LESS_TERMCAP_ue="$(tput rmul; tput sgr0)";
        LESS_TERMCAP_mr="$(tput rev)";
        LESS_TERMCAP_mh="$(tput dim)";
        LESS_TERMCAP_ZN="$(tput ssubm)";
        LESS_TERMCAP_ZV="$(tput rsubm)";
        LESS_TERMCAP_ZO="$(tput ssupm)";
        LESS_TERMCAP_ZW="$(tput rsupm)";
      };
      shellAliases = {
        e = "emacsclient";
        SS = "sudo systemctl";
        f = "$FILE";
        g = "git";
        ls = "ls -hN --color=auto --group-directories-first";
        diff = "diff --color=auto";
      };
    };
  };
}

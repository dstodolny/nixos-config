{ pkgs, ... }:

{
  imports = [
    ./base.nix
  ];

  programs = {
    home-manager = {
      enable = true;
    };
    emacs = {
      enable = true;
      extraPackages = epkgs: with epkgs; [
        company
        pinentry
        rainbow-mode
        undo-tree
        pdf-tools
        bash-completion
        circe
        csv-mode
        daemons
        desktop-environment
        elfeed
        esh-autosuggest
        expand-region
        exwm
        fish-completion
        flycheck
        geiser
        hackernews
        haskell-mode
        helm
        helm-company
        helm-descbinds
        helm-eww
        helm-flycheck
        helm-ls-git
        helm-pass
        helm-slime
        helm-system-packages
        helpful
        hl-todo
        iedit
        js2-mode
        ledger-mode
        lispy
        lispyville
        lua-mode
        magit
        magit-todos
        markdown-mode
        nhexl-mode
        nix-mode
        org
        org-bullets
        org-plus-contrib
        orgit
        pulseaudio-control
        prettier-js
        rainbow-delimiters
        slime
        slime-company
        tide
        typescript-mode
        web-mode
        ztree
      ];
    };
  };

  services = {
#    emacs.enable = true;
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
      defaultCacheTtl = 86400;
      maxCacheTtl = 86400;
      defaultCacheTtlSsh = 86400;
      maxCacheTtlSsh = 86400;
      extraConfig = ''
        pinentry-program "${pkgs.pinentry_emacs}/bin/pinentry-emacs"
        allow-emacs-pinentry
        allow-loopback-pinentry
      '';
    };
  };

  nixpkgs.config.allowUnfree = true;

  services.network-manager-applet.enable = true;

  home = {
    sessionVariables = {
      PATH = "~/.local/npm/bin:$PATH";
      GPG_TTY = "$(tty)";
    };
    file = {
      ".Xresources".source = ../assets/.Xresources;
      ".emacs.d".source = ../assets/.emacs.d;
      ".conkyrc".source = ../assets/.conkyrc;
      ".gitconfig".source = ../assets/.gitconfig;
      "bin".source = ../assets/bin;
      ".gnupg/gpg.conf".source = ../assets/.gnupg/gpg.conf;
      ".xsession" = {
        source = ../assets/.xsession;
        executable = true;
      };
      ".xinitrc".source = ../assets/.xinitrc;
      ".exwm".source = ../assets/.exwm;
      ".profile".source = ../assets/.profile;

      # npm
      ".npmrc".text = ''
        prefix = ~/.local/npm
      '';
    };

    packages = with pkgs; [
      firefox
      chromium
      pass-otp
      nodejs-10_x
      mlocate

      dzen2
      conky

      # anki
      anki

      # haskell
      ghc
      stack
      hlint

      # unfree
      slack
      zoom-us
    ];
  };
}

{ pkgs, ... }:

{
  programs = {
    home-manager = {
      enable = true;
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
      extraConfig = ''
        pinentry-program ~/.nix-profile/bin/pinentry-emacs
        allow-emacs-pinentry
        allow-loopback-pinentry
      '';
    };
  };

  nixpkgs.config.allowUnfree = true;

  home = {
    file = {
      ".Xresources".source = ../assets/.Xresources;
      ".emacs.d".source = ../assets/.emacs.d;
      ".conkyrc".source = ../assets/.conkyrc;
      ".gitconfig".source = ../assets/.gitconfig;
      "bin".source = ../assets/bin;
      ".gnupg/gpg.conf".source = ../assets/.gnupg/gpg.conf;

      # npm
      ".npmrc".text = ''
        prefix = ~/.local/npm
      '';
    };

    packages = with pkgs; [
      firefox
      chromium
      pinentry_emacs
      pass-otp
      nodejs-10_x
      mlocate

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

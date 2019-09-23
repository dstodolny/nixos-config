{ pkgs, ... }:

{
  imports = [ ../modules/module-list.nix ];
  programs.home-manager.enable = true;
  home.file = {
    ".profile".source = ../assets/profile;
  };
  home.packages = with pkgs; [
    fzf
    htop
    lf
    openssl
    pandoc
    pass-otp
    ripgrep
    wget
    youtube-dl
  ];
}

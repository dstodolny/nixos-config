{ pkgs, ... }:

{
  imports = [ ../modules/module-list.nix ];
  programs.home-manager.enable = true;
  home.file = {
    ".profile".source = ../assets/profile;
  };
  home.packages = with pkgs; [
    htop
    fzf
    openssl
    pass-otp
    wget
    ripgrep
    pandoc
    lf
  ];
}

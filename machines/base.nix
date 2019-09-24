{ pkgs, ... }:

{
  imports = [ ../modules/module-list.nix ];
  programs.home-manager.enable = true;
  home = {
    file = {
      "bin".source = ../assets/bin;
    };
    packages = with pkgs; [
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
  };
}

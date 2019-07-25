{ pkgs, ... }:

{
  imports = [ ../modules/module-list.nix ];
  programs = {
    home-manager = {
      enable = true;
    };
  };
  home.file.".nix-channels".source = ../assets/nix-channels;
  home.packages = with pkgs; [
    gnupg
    pinentry_emacs
  ];
}

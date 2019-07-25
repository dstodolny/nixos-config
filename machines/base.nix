{ pkgs, ... }:

{
  imports = [ ../modules/module-list.nix ];
  programs.home-manager.enable = true;
  home.file = {
#    ".nix-channels".source = ../assets/nix-channels;
    ".profile".source = ../assets/profile;
  };
  home.packages = with pkgs; [
    mlocate
  ];
}

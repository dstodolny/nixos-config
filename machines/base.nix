{ pkgs, ... }:

{
  imports = [ ../modules/module-list.nix ];
  programs.home-manager.enable = true;
  home.file = {
    ".profile".source = ../assets/profile;
    ".bashrc".source = ../assets/bashrc;
    ".config/zathura/zathurarc".source = ../assets/zathurarc;
  };
  home.packages = with pkgs; [
    fzf
    openssl
    wget
    zathura
    sxiv
    ripgrep
    pandoc
    tabbed
    lf
  ];
}

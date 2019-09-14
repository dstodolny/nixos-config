{ pkgs, ... }:

{
  imports = [ ../modules/module-list.nix ];
  programs.home-manager.enable = true;
  home.file = {
    ".profile".source = ../assets/profile;
    ".bashrc".source = ../assets/bashrc;
    ".config/zathura/zathurarc".source = ../assets/zathurarc;
    ".config/vifm/vifmrc".source = ../assets/vifmrc;
  };
  programs.fzf.enable = true;
  home.packages = with pkgs; [
    openssl
    wget
    zathura
    vifm
    sxiv
    ripgrep
  ];
}

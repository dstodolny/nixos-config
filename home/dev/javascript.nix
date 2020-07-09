{ pkgs, ... }:

{
  home = {
    file.".npmrc".text = ''
      prefix = ~/.local/npm
    '';
    sessionVariables.PATH = "/home/dnixty/.local/npm/bin:$PATH";
    packages = with pkgs; [
      nodejs-10_x
    ];
  };
}

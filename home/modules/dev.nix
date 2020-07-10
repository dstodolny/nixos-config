{ pkgs, ... }:

{
  home = {
    file = {
      ".npmrc".text = ''
        prefix = ~/.local/npm
      '';
    };
    packages = with pkgs; [
      binutils
      gcc
      gdb
      gnumake
      valgrind
      nodejs-10_x
      sbcl
      clisp
      mitscheme
    ];
    sessionVariables.PATH = "/home/dnixty/.local/npm/bin:$PATH";
  };
  programs.git = {
    enable = true;
    userName = "Dominik Stodolny";
    userEmail = "dominik@stodolny.org";
    signing = {
      key = "0D5591D3B4BB7818";
      signByDefault = true;
    };
    aliases = {
      ci = "commit --signoff";
      co = "checkout";
      st = "status";
    };
    extraConfig = {
      pull = {
        rebase = true;
      };
      push = {
        default = "current";
        recurseSubmodules = "check";
      };
      rebase = {
        autosquash = true;
      };
    };
  };
}

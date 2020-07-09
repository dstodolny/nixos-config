{ pkgs, ... }:

{
  home.packages = with pkgs; [
    binutils
    gcc
    gdb
    gnumake
    valgrind
  ];
}

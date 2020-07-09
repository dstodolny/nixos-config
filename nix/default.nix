let
  sources = import ./sources.nix;
in
rec {
  home-manager = import (sources.home-manager + "/nixos");
  nixpkgs = import sources.nixpkgs;
  emacs = import sources.emacs-overlay;
  nixos-hardware = import sources.nixos-hardware;
}

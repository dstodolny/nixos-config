let
  sources = import ./sources.nix;
in
rec {
  emacs = import sources.emacs-overlay;
}

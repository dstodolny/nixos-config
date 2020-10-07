let
  # home-manager = (import ./nix/sources.nix).home-manager;
  secrets = import ./secrets.nix;
in
{
  programs = {
    home-manager = {
      enable = true;
      # path = "${home-manager}";
    };
  };
  nixpkgs.overlays = [ (import ./nix).emacs ];
  imports = [ (./home + "/${secrets.hostname}.home.nix") ];
}

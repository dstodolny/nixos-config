{ ... }:

let
  secrets = import ./secrets.nix;
in
{
  imports =
    [
      ./hardware-configuration.nix
      (./systems + "/${secrets.hostname}.nix")
    ];
}

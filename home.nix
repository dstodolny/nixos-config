{ pkgs, ... }:

let
  secrets = import ./secrets.nix;
in
{
  imports = [
    (./machines + "/${secrets.hostname}.nix")
  ];
}

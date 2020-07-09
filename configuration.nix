{ ... }:

let
  hostname = "${builtins.readFile ./hostname}";
in
{
  imports =
    [
      ./hardware-configuration.nix
      (./systems + "/${hostname}.nix")
    ];
}

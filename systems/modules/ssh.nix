{ ... }:

let
  shared = import ../../shared.nix;
in
{
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
    permitRootLogin = "no";
    ports = [ shared.ports.ssh ];
  };
}

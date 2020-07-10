{ config, lib, ... }:

with lib;
let
  shared = ../../shared.nix;
in
{
  boot.cleanTmpDir = true;
  i18n.defaultLocale = "en_GB.UTF-8";
  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };
  users.users.dnixty = {
    isNormalUser = true;
    uid = 1000;
    createHome = true;
    group = "users";
    home = "/home/dnixty";
    extraGroups = [ "wheel" "input"]
                  ++ optionals config.services.xserver.enable ["audio" "video" "lp" "networkmanager"];
    openssh.authorizedKeys.keys = shared.ssh_keys;
  };
}

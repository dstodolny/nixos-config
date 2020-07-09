{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.base;
  shared = ../../shared.nix;
in
{
  options = {
    profiles.base = {
      enable = mkOption {
        default = true;
        description = "Enable base profile";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    boot.extraModulePackages = [ config.boot.kernelPackages.wireguard ];
    environment.systemPackages = with pkgs; [
      unstable.wireguard
    ];
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
                    ++ optionals config.profiles.desktop.enable ["audio" "video" "lp" "networkmanager"];
      openssh.authorizedKeys.keys = shared.ssh_keys;
    };
  };
}

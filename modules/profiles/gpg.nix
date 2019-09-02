{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.gpg;
in
{
  options = {
    profiles.gpg = {
      enable = mkOption {
        default = true;
        description = "Enable gpg profile and configuration";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    home = {
      packages = with pkgs; [
        gnupg
        pinentry
      ];
      sessionVariables = {
        GPG_TTY = "$(tty)";
      };
      file.".gnupg/gpg.conf".source = ../../assets/gpg.conf;
    };
    services = {
      gpg-agent = {
        enable = true;
        enableSshSupport = true;
        defaultCacheTtl = 86400;
        maxCacheTtl = 86400;
        defaultCacheTtlSsh = 86400;
        maxCacheTtlSsh = 86400;
        extraConfig = ''
pinentry-program "${pkgs.pinentry}/bin/pinentry"
        '';
      };
    };
  };
}

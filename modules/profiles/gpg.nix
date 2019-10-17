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
        pinentry
      ];
      sessionVariables = {
        GPG_TTY = "$(tty)";
      };
    };
    programs.gpg = {
      enable = true;
      settings = {
        personal-cipher-preferences = "AES256 AES192 AES";
        personal-digest-preferences = "SHA512 SHA384 SHA256";
        personal-compress-preferences = "ZLIB BZIP2 ZIP Uncompressed";
        default-preference-list = "SHA512 SHA384 SHA256 AES256 AES192 AES ZLIB BZIP2 ZIP Uncompressed";
        cert-digest-algo = "SHA512";
        s2k-digest-algo = "SHA512";
        s2k-cipher-algo = "AES256";
        charset = "utf-8";
        fixed-list-mode = true;
        no-comments = true;
        no-emit-version = true;
        keyid-format = "0xlong";
        list-options = "show-uid-validity";
        verify-options = "show-uid-validity";
        with-fingerprint = true;
        require-cross-certification = true;
        no-symkey-cache = true;
        throw-keyids = true;
        use-agent = true;
      };
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

{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [ pinentry ];
  };
  programs.gpg = {
    enable = true;
  };
  services = {
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
      defaultCacheTtl = 86400;
      maxCacheTtl = 86400;
      defaultCacheTtlSsh = 86400;
      maxCacheTtlSsh = 86400;
    };
  };
}

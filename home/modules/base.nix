{ pkgs, ... }:

let
  shared = import ../../shared.nix;
in
{
  home = {
    stateVersion = "20.03";
    packages = with pkgs; [
      file
      ledger
      niv
      nix-index
      patchelf
      pinentry
      ripgrep
      transmission
      unzip
      wget
    ];
    file = {
      ".profile".text = ''
        . "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
        if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then exec startx; exec exit; fi
'';
    };
  };
  programs = {
    ssh = {
      enable = true;
      matchBlocks = {
        "njord" = {
          hostname = "njord";
          user = "dnixty";
          port = shared.ports.ssh;
        };
        "njord.vpn" = {
          hostname = "njord.vpn";
          user = "dnixty";
          port = shared.ports.ssh;
        };
      };
    };
    gpg.enable = true;
    password-store = {
      enable = true;
      package = pkgs.pass.withExtensions (exts: [ exts.pass-otp ]);
      settings = { PASSWORD_STORE_KEY = "dominik@stodolny.org"; };
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
    };
    password-store-sync.enable = true;
  };
}

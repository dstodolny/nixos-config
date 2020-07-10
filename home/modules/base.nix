{ pkgs, ... }:

{
  home = {
    stateVersion = "20.03";
    packages = with pkgs; [
      transmission
      patchelf
      unzip
      wget
      pass-otp
      ledger
      niv
      pinentry
    ];
    file = {
      ".profile".text = ''
        . "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
        if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then exec startx; exec exit; fi
'';
    };
  };
  programs.gpg.enable = true;
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

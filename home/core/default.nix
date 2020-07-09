{ pkgs, ... }:

{
  imports = [
    ./emacs.nix
    ./git.nix
    ./gpg.nix
    ./wechall.nix
  ];

  home = {
    stateVersion = "20.03";
    packages = with pkgs; [
      transmission
      patchelf
      unzip
      wget
      pass-otp
      ledger
    ];
    file = {
      ".profile".source = ../assets/profile;
    };
  };
}

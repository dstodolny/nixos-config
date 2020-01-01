{ ... }:

{
  imports = [
    ./base.nix
  ];

  profiles = {
    bitcoin.enable = true;
    dev.scheme.enable = true;
    desktop.exwm.enable = true;
    finance.enable = true;
    pass.enable = true;
  };
}

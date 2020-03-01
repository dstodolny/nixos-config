{ ... }:

{
  imports = [
    ./base.nix
  ];

  profiles = {
    bitcoin.enable = true;
    dev = {
      scheme.enable = true;
      lisp.enable = true;
    };
    gfx.enable = true;
    desktop.exwm.enable = true;
    finance.enable = true;
    im.enable = true;
    pass.enable = true;
  };
}

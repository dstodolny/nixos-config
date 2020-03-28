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
      c.enable = true;
    };
    gfx.enable = true;
    desktop.exwm.enable = true;
    finance.enable = true;
    im.enable = true;
    pass.enable = true;
  };
}

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
      javascript.enable = true;
    };
    gfx.enable = true;
    desktop.exwm.enable = true;
    finance.enable = true;
    im.enable = true;
    pass.enable = true;
    work.enable = true;
    wechall.enable = true;
  };
}

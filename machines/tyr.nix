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
    desktop.xfce.enable = true;
    emacs.enable = true;
    finance.enable = true;
    gfx.enable = true;
    im.enable = true;
    pass.enable = true;
    wechall.enable = true;
  };
}

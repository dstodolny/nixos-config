{ ... }:

{
  imports = [
    ./base.nix
  ];

  profiles = {
    dev.javascript.enable = true;
    desktop.exwm.enable = true;
    work.enable = true;
    pass.enable = true;
  };
}

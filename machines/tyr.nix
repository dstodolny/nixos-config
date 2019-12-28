{ ... }:

{
  imports = [
    ./base.nix
  ];

  profiles = {
    dev.scheme.enable = true;
    desktop.exwm.enable = true;
    pass.enable = true;
    vim.enable = true;
  };
}

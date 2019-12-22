{ ... }:

{
  imports = [
    ./base.nix
  ];

  profiles = {
    desktop.exwm.enable = true;
    pass.enable = true;
    vim.enable = true;
  };
}

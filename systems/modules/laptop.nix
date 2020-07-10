{ ... }:

{
  services.acpid.enable = true;
  powerManagement.enable = true;
  networking.networkmanager.enable = true;
  time.timeZone = "Europe/London";
}

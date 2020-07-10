{ pkgs, ... }:

let
  shared = import ../../shared.nix;
  username = "dnixty";
in
{
  systemd = {
    targets."autologin-tty1" = {
      description = "Final";
      requires = [ "multi-user.target" ];
      after = [ "multi-user.target" ];
      unitConfig = { AllowIsolate = "yes"; };
    };
    services = {
      "supress-kernel-logging" = {
        description = "Suppress kernel logging to the console";
        after = [ "multi-user.target" ];
        wantedBy = [ "autologin-tty1.target" ];
        serviceConfig = {
          ExecStart = [
            "${pkgs.utillinux}/sbin/dmesg -n 1"
          ];
          Type = "oneshot";
        };
      };
      "autovt@tty1" = {
        description = "Autologin on TTY1";
        after = [ "suppress-kernel-logging.service" ];
        wantedBy = [ "autologin-tty1.target" ];
        restartIfChanged = false;
        serviceConfig = {
          ExecStart = [
            "" # override upstream default with an empty ExecStart
            "@${pkgs.utillinux}/sbin/agetty agetty --login-program ${pkgs.shadow}/bin/login --autologin ${username} --noclear %I $TERM"
          ];
          Restart = "always";
          Type = "idle";
        };
      };
    };
  };
}

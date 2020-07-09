{ ... }:

let
  sources = import ../../nix/sources.nix;
in
{
  imports = [
    (sources.nixos-hardware + "/lenovo/thinkpad")
    (sources.nixos-hardware + "/common/cpu/intel")
    (sources.nixos-hardware + "/common/pc/ssd")
  ];
  boot = {
    kernelParams = [ "intel_idle.max_cstate=1" ];
  };
  hardware.cpu.intel.updateMicrocode = true;
}

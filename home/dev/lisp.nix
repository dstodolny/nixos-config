{ pkgs, ... }:

{
  home = {
    file = {
      ".sbclrc".source = ../assets/sbclrc;
    };
    packages = with pkgs; [
      sbcl
      clisp
      lispPackages.quicklisp
      lispPackages.cl_plus_ssl
    ];
  };
}

{ ... }:

{
  networking.firewall.allowedTCPPorts = [ 80 443 ];

  security = {
    acme = {
      acceptTerms = true;
      email = "dominik@stodolny.org";
    };
  };
  services = {
    nginx = {
      enable = true;
      virtualHosts."dnixty.com" = {
        enableACME = true;
        forceSSL = true;
        root = "/var/www/dnixty.com";
      };
    };
  };
}

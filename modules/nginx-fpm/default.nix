{ dataDir, documentRoot ? "/var/www/html", domain, inputs, pkgs, port ? "9000", ... }:
{
  imports = [
    "${inputs.self}/modules/nginx-acme-base"
  ];
  services.nginx = {
    appendHttpConfig = ''
      index index.php;
    '';
    virtualHosts."${domain}" = {
      enableACME = true;
      forceSSL = true;
      root = dataDir;
      locations = {
        "~ \\.php$" = {
          extraConfig = ''
            fastcgi_split_path_info ^(.+?\.php)(/.*)$;
            include ${pkgs.nginx}/conf/fastcgi_params;
            include ${pkgs.nginx}/conf/fastcgi.conf;
            fastcgi_param  SCRIPT_FILENAME  ${documentRoot}$fastcgi_script_name;
            fastcgi_index index.php;
            fastcgi_pass 127.0.0.1:${port};
          '';
        };
      };
    };
  };
}

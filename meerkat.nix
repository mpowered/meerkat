{ lib, pkgs, config, ... }:
with lib;
let
  cfg = config.services.meerkat;
  src = builtins.fetchTarball {
    url = https://github.com/mpowered/meerkat/archive/REV.tar.gz;
    sha256 = "SHA";
  };
  meerkat = (import src {}).meerkat;
  meerkatConfig = pkgs.writeText "meerkat.yaml" ''
host: ${cfg.host}
logging:
  # logfile: "meerkat.log"
  loglevel: info
database:
  host: ${cfg.database.host}
  port: ${toString cfg.database.port}
  user: ${cfg.database.user}
  password: ${cfg.database.password}
  database: ${cfg.database.database}
binaries:
  df: ${pkgs.coreutils}/bin/df
  free: ${pkgs.procps}/bin/free
  pidstat: ${pkgs.sysstat}/bin/pidstat
'';

in {
  options.services.meerkat = {
    enable = mkEnableOption "meerkat service";
    host = mkOption {
      type = types.string;
      description = ''Host that identifies this system to report with metrics.'';
    };
    database = {
      host = mkOption {
        type = types.string;
        default = "localhost";
        description = ''Host where the Meerkat postgresql database can be found.'';
      };
      port = mkOption {
        type = types.int;
        default = 5432;
        description = ''Port where the Meerkat postgresql database can be found.'';
      };
      user = mkOption {
        type = types.string;
        default = "meerkat";
        description = ''User to use when authorizing with the Meerkat postgresql database.'';
      };
      password = mkOption {
        type = types.string;
        description = ''Password to use when authorizing with the Meerkat postgresql database.'';
      };
      database = mkOption {
        type = types.string;
        description = ''Database name of the Meerkat postgresql database.'';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.meerkat = {
      after = [ "postgresql.service" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = "${meerkat}/bin/meerkat -c ${meerkatConfig}";
      };
    };
  };
}

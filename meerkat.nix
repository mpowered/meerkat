{ lib, pkgs, config, ... }:
with lib;
let
  cfg = config.services.meerkat;
  meerkat = self.callPackage ./. {};
in {
  options.services.meerkat = {
    enable = mkEnableOption "meerkat service";
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

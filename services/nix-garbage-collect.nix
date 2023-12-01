{ config, lib, pkgs, ...}:

with lib;

let
  cfg = config.services.nix-garbage-collect;
in {
  options = {
    services.nix-garbage-collect = {
      enable = mkOption {
        default = false;
        description = ''
          Enable nix garbage collection
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.user.timers.nix-garbage-collect = {
      Unit = {
        Description = "Run nix garbage collection";
      };

      Timer = {
        OnCalendar = "daily";
        Persistent = true;
        Unit = "nix-garbage-collect.service";
      };

      Install = {
        WantedBy = [ "timers.target" ];
      };
    };

    systemd.user.services.nix-garbage-collect = {
       Unit = {
         Description = "Run nix garbage collection";
       };

       Install = {
         WantedBy = [ "default.target" ];
       };

       Service = {
         ExecStart = "/nix/var/nix/profiles/default/bin/nix-store --gc";
       };
    };

  };
}

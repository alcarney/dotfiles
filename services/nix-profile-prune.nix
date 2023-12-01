{ config, lib, pkgs, ...}:

with lib;

let
  cfg = config.services.nix-profile-prune;
in {
  options = {
    services.nix-profile-prune = {
      enable = mkOption {
        default = false;
        description = ''
          Enable profile pruning
        '';
      };
    };
  };

  config =
   let
     pruneProfiles = pkgs.writeShellScript "nix-prune-profiles.sh" ''
       set -euo pipefail

       # TODO: Is there a nicer way to reference the nix command?
       /nix/var/nix/profiles/default/bin/nix profile wipe-history --older-than 30d
       $HOME/.nix-profile/bin/home-manager expire-generations "-30 days"
       '';
   in

  mkIf cfg.enable {
    systemd.user.timers.nix-profile-prune = {
      Unit = {
        Description = "Delete old nix profile generations";
      };

      Timer = {
        OnCalendar = "daily";
        Persistent = true;
        Unit = "nix-profile-prune.service";
      };

      Install = {
        WantedBy = [ "timers.target" ];
      };
    };

    systemd.user.services.nix-profile-prune = {
       Unit = {
         Description = "Delete old nix profile generations";
       };

       Install = {
         WantedBy = [ "default.target" ];
       };

       Service = {
         ExecStart = "${pruneProfiles}";
       };
    };

  };
}

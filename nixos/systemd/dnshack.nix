{ config, pkgs, lib, host,... }:
let

  # PATH="/run/current-system/sw/bin:${pkgs.home-manager}/bin:$PATH"
  mkStartScript = name: pkgs.writeShellScript "${name}.sh" ''
        set -euo pipefail
        printf $PATH
        export NIX_PATH="nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos:nixos-config=/etc/nixos/configuration.nix"
        resolvectl dns wlan0 192.168.1.224
        resolvectl dns tailscale0 100.94.223.34
      '';
in {

  systemd.timers = {
    "dns-hack" = {
      wantedBy = [ "timers.target" ];
      enable = true;
      timerConfig = {
        OnBootSec= "25s";
        Unit = "dns-hack.service";
        OnCalendar = "5m";
      };
    };
  };
  systemd.services = {
    "dns-hack" = {
      description = "take snapshot of home directory";
      serviceConfig.Type = "oneshot";
      path=[
        "/run/current-system/sw/"
        "/run/wrappers/"
      ];
      script = "${ mkStartScript "dnshack" }";
      wantedBy = [ "multi-user.target" ];
      after = ["tailscaled.service" ];
      wants = ["tailscaled.service" ];
    };

  };


}

{ config, pkgs, ... }:
{
    # system.autoUpgrade.allowReboot = true;

    # Mount secondary drive
    fileSystems."/home/jonathan/Storage".device = "/dev/disk/by-label/storage";
    fileSystems."/home/jonathan/Storage".options = ["compress=zstd"];
    boot.kernelPackages = pkgs.linuxPackages_zen;

    environment.systemPackages = with pkgs; [
        btrfs-progs
        intel-gpu-tools
    ];
    # services.flatpak.enable = true;
    services.xserver.displayManager.gdm.autoSuspend= false;
    # xdg.portal.enable = true;
    # xdg.portal.extraPortals = [
    #     pkgs.xdg-desktop-portal-gnome
    # ];

    swapDevices = [
        {
            device = "/swapfile";
            size = 4096;
        }
    ];

    hardware.opengl = {
        enable = true;
        extraPackages = with pkgs; [
            intel-media-driver # LIBVA_DRIVER_NAME=iHD
            # vaapiIntel       # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
            vaapiVdpau
            libvdpau-va-gl
        ];
    };

    security.sudo.extraRules = [
        {
            users = [ "jonathan" "tomg" ];
            commands = [
                { command = "/opt/receive-backup"; options = [ "NOPASSWD" ]; }
                { command = "/opt/dad-backup"; options = [ "NOPASSWD" ]; }
            ];
        }
    ];

    systemd.timers = {
        "scrub" = {
            wantedBy = [ "timers.target" ];
            enable = false;
            after = [ "time-set.target" "time-sync.target" ];
            timerConfig = {
                OnBootSec= "5m";
                Unit = "btrfs-scrub.service";
                OnCalendar = "monthly";
            };
        };

        "status" = {
            wantedBy = [ "timers.target" ];
            enable = false;
            after = [ "time-set.target" "time-sync.target" ];
            timerConfig = {
                Unit = "disk-check.service";
                OnCalendar = "*-*-02 00:00:00";
            };

        };

        "snap" = {
            wantedBy = [ "timers.target" ];
            enable = true;
            timerConfig = {
                OnActiveSec= "daily";
                OnBootSec= "5m";

                Unit = "home-snapshot.service";
            };

        };

    };

    systemd.services = {
        "btrfs-scrub" = {
            description = "Run btrfs scrub monthly";
            script = "/run/current-system/sw/bin/btrfs scrub start /dev/disk/by-label/storage";
            wantedBy = [ "scrub.timer" ];
        };

        "disk-check" = {
            description = "Check status of btrfs scrub";
            script = "/run/current-system/sw/bin/btrfs scrub status /dev/disk/by-label/storage > /home/disk-check";
            wantedBy = [ "status.timer" ];
        };

        "home-snapshot" = {
            description = "take snapshot of home directory";
            script = "/run/current-system/sw/bin/btrfs subvolume snapshot -r /home/jonathan/Storage /home/jonathan/Storage/.snapshots/$(run/current-system/sw/bin/date -Ihour) ";
            wantedBy = [ "snap.timer" ];
        };
    };

    services.apcupsd.enable = true;


      users.users.tomg = {
          isNormalUser = true;
          home  = "/home/tomg";
          createHome  = true;
          # extraGroups = [ "audio" "video" "kvm" ];
          shell = pkgs.zsh;
          initialPassword = "v/d7xBMgrZnLO[H[W`6z:3Ru@}";
      };
}

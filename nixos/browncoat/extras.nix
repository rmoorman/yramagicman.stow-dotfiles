{ config, pkgs, ... }:
{
    system.autoUpgrade.allowReboot = true;

    # Mount secondary drive
    fileSystems."/home/jonathan/Storage".device = "/dev/disk/by-label/storage";
    fileSystems."/home/jonathan/Storage".options = ["compress=zstd"];
    boot.kernelPackages = pkgs.linuxPackages_zen;

    environment.systemPackages = with pkgs; [
        dropbox
        btrfs-progs
        intel-gpu-tools
    ];
    services.flatpak.enable = true;
    services.xserver.displayManager.gdm.autoSuspend= false;
    xdg.portal.enable = true;
    xdg.portal.extraPortals = [
        pkgs.xdg-desktop-portal-gnome
    ];

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
            users = [ "jonathan" ];
            commands = [ { command = "/opt/receive-backup"; options = [ "NOPASSWD" ]; } ];
        }
    ];

    systemd.timers = {
        "scrub" = {
            wantedBy = [ "timers.target" ];
            enable = false;
            after = [ "time-set.target" "time-sync.target" ];
            timerConfig = {
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
    };

    services.netdata.enable = true;

}

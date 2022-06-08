{ config, pkgs, ... }:
{
    # boot.kernelPackages = pkgs.linuxPackages_zen;
    fileSystems."/".options = ["compress=zstd"];
    environment.systemPackages = with pkgs; [
        dropbox
        microcodeAmd
        xorg.xf86videoamdgpu
        openrgb
    ];

    services.flatpak.enable = true;
    services.xserver.displayManager.gdm.autoSuspend= false;
    xdg.portal.enable = true;
    xdg.portal.extraPortals = [
        pkgs.xdg-desktop-portal-gnome
    ];

    #nixpkgs.config.packageOverrides = pkgs: {
    #    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
    #};

    #hardware.opengl = {
    #    enable = true;
    #    extraPackages = with pkgs; [
    #        intel-media-driver # LIBVA_DRIVER_NAME=iHD
    #        # vaapiIntel       # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
    #        vaapiVdpau
    #        libvdpau-va-gl
    #    ];
    #};

     systemd.timers = {
         "snap" = {
             wantedBy = [ "timers.target" ];
             enable = true;
             timerConfig = {
                 OnBootSec= "1m";
                 Unit = "home-snapshot.service";
                 OnCalendar = "hourly";
             };
         };

         "backup" = {
             wantedBy = [ "timers.target" "network.target" ];
             enable = true;
             timerConfig = {
                 Unit = "backup.service";
                 OnBootSec= "5m";
             };
         };

         "scrub" = {
             wantedBy = [ "timers.target" ];
             after = [ "time-set.target" "time-sync.target" ];
             enable = true;
             timerConfig = {
                 Unit = "btrfs-scrub.service";
                 OnCalendar = "monthly";
                 Persistent=true;
             };
         };

         "status" = {
             wantedBy = [ "timers.target" ];
             after = [ "time-set.target" "time-sync.target" ];
             enable = true;
             timerConfig = {
                 Unit = "disk-check.service";
                 OnCalendar = "monthly";
                 Persistent=true;
             };
         };
     };

     systemd.services = {
         "home-snapshot" = {
             description = "take snapshot of home directory";
             script = "/opt/home-snapshot";
             wantedBy = [ "snap.timer" ];
         };

         "backup" = {
             description = "Backup home directory to local server";
             script = "/opt/home-backup";
             wantedBy = [ "backup.timer" ];
         };

         "btrfs-scrub" = {
             description = "Run btrfs scrub monthly";
             script = "/run/current-system/sw/bin/btrfs scrub start /dev/disk/by-label/nixos";
             wantedBy = [ "scrub.timer" ];
         };

         "disk-check" = {
             description = "Check status of btrfs scrub";
             script = "/run/current-system/sw/bin/btrfs scrub status /dev/disk/by-label/nixos > /home/disk-check";
             wantedBy = [ "status.timer" ];
         };

     };
}

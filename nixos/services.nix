{ config, pkgs, ... }:
{
  # Enable sound.
  sound.enable = true;

  services = {
    yubikey-agent.enable = true;

    tlp.enable = true;
    udisks2.enable = true;
    # Enable CUPS to print documents.
    printing.enable = true;
    avahi = {
        enable = true;
        publish = {
            enable = true;
            workstation = true;
            userServices = true;
        };
        openFirewall = true;
        nssmdns = true;
    };

    pipewire.enable = true;
    pipewire.pulse.enable = true;
    pipewire.alsa.enable = true;
    pipewire.wireplumber.enable = true;
    fwupd.enable = true;
    locate.enable = true;
    locate.locate = pkgs.mlocate;
    locate.interval = "*:0/15";
    locate.localuser = null;


    # Enable the OpenSSH daemon.
    openssh.enable = true;

    # httpd.enable = true;
    # httpd.adminAddr = "yramagicman@gmail.com";
    # httpd.enablePHP = true;
    # mysql.package = pkgs.mariadb;
    # mysql.enable = true;

    phpfpm.phpOptions = ''
            memory_limit = 2048M
    '';

    redshift.enable = true;
    geoclue2.enable = true;
    fstrim.enable = true;
    atd.enable = true;
    netdata.enable = true;
    nscd.enableNsncd = true;
    # enable tailscale
    tailscale.enable=true;
    tailscale.interfaceName="tailscale0";
    lshd.tcpForwarding="true";
    gnome.gnome-keyring.enable = true;
    smartd = {
        enable = true;
        extraOptions = [
            "-A /var/log/smartd/"
            "--interval=3600"
        ];
    };
};
networking.firewall.checkReversePath = "loose";
networking.firewall.allowedTCPPorts = [
    19999
];
security.polkit.enable = true;

location.provider = "geoclue2";
hardware.logitech.wireless.enable = true;
hardware.logitech.wireless.enableGraphical = true;
hardware.pulseaudio.enable = false;
programs.firejail.enable = true;
programs.wireshark.enable = true;
programs.wireshark.package = pkgs.wireshark-qt;

security.rtkit.enable = true;
xdg.portal.enable = true;

}

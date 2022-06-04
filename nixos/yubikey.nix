{ config, pkgs, ... }:
{
    services = {
        udev.packages = [ pkgs.yubikey-personalization ];
        yubikey-agent.enable = true;
        pcscd.enable = true;
    };
    # security.pam.yubico.enable = true;
    # # one of "required", "requisite", "sufficient", "optional"
    # security.pam.yubico.control= "sufficient";
    # security.pam.yubico.id = "32250";

    security.pam.yubico = {
        enable = true;
        # debug = true;
        mode = "challenge-response";
        control= "sufficient";
    };


    environment.systemPackages = with pkgs; [

        yubikey-manager-qt
        yubikey-manager
        yubikey-personalization
        yubioath-desktop
    ];
}

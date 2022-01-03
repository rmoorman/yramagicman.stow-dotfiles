{ config, pkgs, ... }:
{
      # Define a user account. Don't forget to set a password with ‘passwd’.
      users.users.jonathan = {
          isNormalUser = true;
          home  = "/home/jonathan";
          createHome  = true;
          extraGroups = [ "wheel" "audio" "video" "kvm" ]; # Enable ‘sudo’ for the user.
          shell = pkgs.zsh;
          initialPassword = "letmein";
      };
  }

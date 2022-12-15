{ config, pkgs, ... }:
{
      # Define a user account. Don't forget to set a password with ‘passwd’.
      users.users.jonathan = {
          isNormalUser = true;
          home  = "/home/jonathan";
          createHome  = true;
          extraGroups = [ "wheel" "audio" "video" "kvm" "wireshark" ]; # Enable ‘sudo’ for the user.
          shell = pkgs.zsh;
          initialPassword = "letmein";
      };
      users.users.jonathan_backup = {
          isNormalUser = true;
          createHome  = true;
          extraGroups = [ "wheel" "audio" "video" "kvm" "wireshark" ]; # Enable ‘sudo’ for the user.
          shell = pkgs.zsh;
          hashedPassword = "$6$j2ch4akYV/gV5RyG$JktfUeYmQXPf8IOCCXWP9u0Hg6XctInpNQbtB844agEBS7cfrhFNdbuYWYsKJOmF9dD4tVlXjl3MGor34U6Lv.";
      };
  }

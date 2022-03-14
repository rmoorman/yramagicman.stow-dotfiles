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
      users.users.jonathan_backup = {
          isNormalUser = true;
          createHome  = true;
          extraGroups = [ "wheel" "audio" "video" "kvm" ]; # Enable ‘sudo’ for the user.
          shell = pkgs.zsh;
          hashedPassword = "$6$qA3jRpKZbDE2iBi9$wspJReoIAJhHl5nQcU4sVfQwIavFbG5NddkmsObCvXx1mhoEy6y/GlVpLlV4y69LFCOtV5BBfQQ/YCQlIbjva1";
      };
  }

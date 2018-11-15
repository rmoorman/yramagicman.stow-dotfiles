#!/bin/sh
set -e
sudo pacman --needed -S sudo wget
mkdir yay
(
cd yay
wget https://aur.archlinux.org/cgit/aur.git/snapshot/yay.tar.gz
tar -xf yay.tar.gz
cd yay
makepkg -sic
)
rm -rf yay
yay -S --needed $(cat "$HOME/bin/setup/packages" | xargs)

#!/bin/sh
set -e
sudo pacman --needed -S wget
mkdir yay
(
cd yay
wget https://aur.archlinux.org/cgit/aur.git/snapshot/yay.tar.gz
tar -xf yay.tar.gz
cd yay
makepkg -sic
)
rm -rf yay
for p in $(cat ./packages)
do
    sudo pacman -S "$p"
done

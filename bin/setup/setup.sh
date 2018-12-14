#!/bin/sh
sudo pacman --needed -S wget
mkdir yay
(
cd yay || return
wget https://aur.archlinux.org/cgit/aur.git/snapshot/yay.tar.gz
tar -xf yay.tar.gz
cd yay || return
makepkg -sic
)
rm -rf yay
for p in $(cat ./packages)
do
    sudo pacman -S --needed --noconfirm "$p"
done

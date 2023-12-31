#!/bin/sh

partition() {
    if [ -d /sys/firmware/efi/efivars ]
    then
        parted "$1" mklabel gpt
        parted "$1" mkpart primary fat32 1MiB 551MiB
        parted "$1" set 1 esp on
        parted "$1" mkpart primary ext4 551MiB "$2"
        parted "$1" mkpart primary ext4 "$2" 100%
        mkfs.vfat "$1""1"
        mkfs.ext4 "$1""2"
        mkfs.ext4 "$1""3"
    else
    parted "$1" mklabel msdos
        parted "$1" mkpart primary ext4 1MiB "$2"
        parted "$1" set 1 boot on
        parted "$1" mkpart primary ext4 "$2" 100%
        mkfs.ext4 "$1""1"
        mkfs.ext4 "$1""2"
    fi
}

mnt_drives() {
    if [ -d /sys/firmware/efi/efivars ]
    then
        mount "$1""2" /mnt
        mkdir -p /mnt/boot
        mkdir -p /mnt/home
        mount "$1""1" /mnt/boot
        mount "$1""3" /mnt/home
    else
        mount "$1""1" /mnt
        mkdir -p /mnt/home
        mount "$1""2" /mnt/home
    fi
}

timedatectl set-ntp true
lsblk
printf "Please input the drive you wish to partition.\n"
read -r device
printf "Please input the size of your root partition, accepts MiB and GiB\n"
read -r size
partition "$device" "$size"
mnt_drives "$device"
pacstrap /mnt base base-devel linux linux-firmware
genfstab -U /mnt >> /mnt/etc/fstab
git clone https://gitlab.com/yramagicman/stow-dotfiles /mnt/root/stow-dotfiles
arch-chroot /mnt

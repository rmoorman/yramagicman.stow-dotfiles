#/bin/sh

partition() {
    if [ -d /sys/firmware/efi/efivars ]
    then
        echo "parted $1 mklabel gpt"
        echo "parted $1 mkpart primary fat32 1MiB 551MiB"
        echo "parted $1 set 1 esp on"
        echo "parted $1 mkpart primary ext4 551MiB $2"
        echo "parted $1 mkpart primary ext4 $2 100%"
        echo "mkfs.vfat $1 1"
        echo "mkfs.ext4 $1 2"
        echo "mkfs.ext4 $1 3"
    else
        parted "$1" mklabel msdos
        parted "$1" mkpart primary ext4 1MiB "$2"
        parted "$1" set 1 boot on
        parted "$1" mkpart primary ext4 "$2" 100%
        echo mkfs.ext4 "$1""1"
        echo mkfs.ext4 "$1""2"
    fi
}

mnt_drives() {
    true;
}

timedatectl set-ntp true
printf "Please input the drive you wish to partition."
read -r device
printf "Please input the size of your root partition, accepts MiB and GiB"
read -r size
partition "$device" "$size"

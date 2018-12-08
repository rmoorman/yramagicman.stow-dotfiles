set -e
partition() {
    if test -n "$(ls /sys/firmware/efi/efivars)"; then
	parted "$1" mkpart primary fat32 1MiB 551MiB
	parted "$1" set 1 esp on
	parted "$1" mkpart primary ext4 551MiB $2
	parted "$1" mkpart primary ext4 $2 100%
	mkfs.vfat "$1""1"
	mkfs.ext4 "$1""2"
	mkfs.ext4 "$1""3"
    else
	parted $1 mkpart primary ext4 1MiB $2
	parted $1 set 1 boot on
	parted $1 mkpart primary ext4 $2 100%
	mkfs.ext4 "$1""1"
	mkfs.ext4 "$1""2"
    fi
}

if ! curl https://example.com > /dev/null; then
    wifi-menu
    if ! curl https://example.com; then
	printf 'no internet. please reconnect and try again'
	exit
    fi
fi
timedatectl set-ntp true
printf "Please input the drive you wish to partition."
read device
printf "Please input the size of your root partition, accepts MiB and GiB"
read $size
partition($device, $size)

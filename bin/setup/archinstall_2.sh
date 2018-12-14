#!/bin/sh
set -e
ln -sf /usr/share/zoneinfo/America/New_York /etc/localtime
hwclock --systohc
sed -i 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen
locale-gen
echo 'LANG=en_US.UTF-8' > /etc/locale.conf
echo 'please input a hostname'
read -r hstname

echo "$hstname" > /etc/hostname

cat << EOF > /etc/hosts
127.0.0.1   localhost
::1     localhost
127.0.1.1  $hstname
EOF

mkinitcpio -p linux

printf 'set root password \n'
passwd

printf 'input username for non-root user:\n'
read -r username
useradd -m -g users -G wheel -s /bin/sh "$username"
printf 'and password\n'
passwd "$username"

pacman -Syu grub dhcpcd wifi-menu dialog

if [ -d /sys/firmware/efi/efivars ]; then
    pacman -S efibootmgr
    mount /dev/sda1 /boot
    grub-install --target=x86_64-efi --efi-directory=/boot --bootloader-id=GRUB
else
    grub-install --target=i386-pc /dev/sda
fi
systemctl enable dhcpcd
grub-mkconfig -o /boot/grub/grub.cfg
#source ./setup.sh

#!/bin/sh
pacman -D --asdeps $(pacman -Qqe)
pacman -D --asexplicit base base-devel linux linux-firmware zsh
pacman -Rns $(pacman -Qtdq)

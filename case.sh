part="z"
reposdir="$HOME/Documents"
if test -z "$1"; then
    while read line; do
        printf "%s\n" "$line"
    done <<-EOF
    -f) Dot files
    -d) Dot directoriess
    -c) Config directories
    -b) Bin directory
    -r) Make repos directory
    -t) Build st
    -w) Build dwm
    -s) Set shell
    -j) Install cron jobs
    -a) Installs everything
    -n) Build nix config from files
    -v) install nvim directory
EOF
fi
while test "$1"; do
    case $1 in
        -f)
            $part="files";
            ;;
        -d)
            dotdirs
            ;;
        -c)
            configdir
            ;;
        -b)
            bindir
            systemd
            ;;
        -r)
            mkrepodir
            ;;
        -t)
            st
            ;;
        -w)
            dwm
            ;;
        -s)
            setshell
            ;;
        -j)
            cron
            ;;
        -v)
            set_nvim
            ;;
        -n)
            $part=nixos
            ;;
        -a)
            bindir
            configdir
            my_nix
            cron
            dotdirs
            dotfiles
            dwm
            mkrepodir
            setshell
            st
            ;;
        --)
            githooks
            clean
            break;;
    esac
done





for f in * ;
do
    case $f in
        *.conf|*.txt|*.sh|*.md) ;;
        dwm.config.h)
            [ $part == 'dwm' ] && {

                if test "$(uname)" != "Darwin"
                then
                    if [ ! -d "$reposdir/dwm/" ]
                    then
                        (
                            echo cd "$reposdir" || return
                            echo git clone --depth 3 git://git.suckless.org/dwm
                            echo cp "$PWD/dwm.config.h" "$reposdir/dwm/config.h"
                            echo cd "$reposdir/dwm" || return
                            echo make
                            echo sudo make install
                        )
                    else
                        (
                            echo cp "$PWD/dwm.config.h" "$reposdir/dwm/config.h"
                            echo cd "$reposdir/dwm" || return
                            echo make clean
                            echo make
                            echo sudo make install
                        )
                    fi
                fi
            }
            ;;

        st.config.h)
            [ $part == 'st' ] && {
                if test "$(uname)" != "Darwin"
                then
                    if [ ! -d "$reposdir/st/" ]
                    then
                        (
                            echo cd "$reposdir" || return
                            echo git clone --depth 3 git://git.suckless.org/st
                            echo cp "$PWD/st.config.h" "$reposdir/st/config.h"
                            echo cd "$reposdir/st" || return
                            echo make
                            echo sudo make install
                        )
                    else
                        (
                            echo cp "$PWD/st.config.h" "$reposdir/st/config.h"
                            echo cd "$reposdir/st" || return
                            echo make clean
                            echo make
                            echo sudo make install
                        )
                    fi
                fi
            }
            ;;

            bin) [ $part == 'bin' ] && echo ln -sfv "$PWD/$f" "$HOME/.local/$f" ;;
            vim) [ $part == 'vim' ] && {
                echo ln -sfv "$PWD/$f" "$HOME/.local/$f"
                echo ln -sfv "$PWD/$f" "$HOME/.config/n$f"
            }
                ;;
            systemd) [ $part == 'systemd' ] && echo ln -sfv "$PWD/$f" "$HOME/.local/$f" ;;
            xmonad) [ $part == 'xmonad' ] && echo ln -sfv "$PWD/$f" "$HOME/$f" ;;
            config) [ $part == 'config' ] && {
                for d in config/*;
                do
                    echo ln -sfv "$PWD/$d" "$HOME/$d"
                done
            } ;;
            nixos) [ $part == 'nixos' ] && [ -d $f ] && [ $f == 'nixos' ] && {
                echo "building configuration.nix"
                echo "backing up configuration.nix in place"
                echo sudo cp -v /etc/nixos/configuration.nix /etc/nixos/configuration.nix.bak
                echo sudo cp -v "$PWD"/nixos/configuration.nix /etc/nixos/configuration.nix
                echo "generating configuration.nix as /tmp/configuration.nix"
                echo sudo ln -s "$PWD"/nixos/header.nix /etc/nixos/header.nix
                echo sudo ln -s "$PWD"/nixos/network."$(hostname)".nix /etc/nixos/network.nix
                echo sudo ln -s "$PWD"/nixos/packages.nix /etc/nixos/packages.nix
                echo sudo ln -s "$PWD"/nixos/fonts.nix /etc/nixos/fonts.nix
                echo sudo ln -s "$PWD"/nixos/services.nix /etc/nixos/services.nix
                echo sudo ln -s "$PWD"/nixos/users.nix /etc/nixos/users.nix
                echo sudo ln -s "$PWD"/nixos/extras."$(hostname)".nix /etc/nixos/extras.nix
                echo sudo ln -s "$PWD"/nixos/footer.nix /etc/nixos/footer.nix
            }

            ;;

        *) [ $part == 'files' ] && [ -f $f ] && echo ln -sfv "$PWD/$f" "$HOME/$f"
    esac
done

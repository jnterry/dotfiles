#/usr/bin/bash
# Sets up new debian machine with all required packages

set -e

# sudo apt update

# Ensure third party repos in config are cloned, eg, zsh plugins
git submodule init
git submodule update

# dependencies for this script (and useful to keep on system anyway...)
sudo apt install --yes fontconfig rsync curl gnupg

# Copy file structure
sudo rsync -r ./files/* /
/usr/bin/fc-cache -f -v # update font cache (fonts installed by above cp)
mkdir -p /etc/network/interfaces.d
sudo chmod +x /usr/bin/stowforce
sudo chmod +x /usr/bin/i3screenshot
sudo chmod +x /usr/bin/i3screenshot-full
sudo chmod +X /usr/bin/i3emptyworkspace
sudo chmod +x /usr/bin/git-change-origin

# Import apt keys (new repos added with rsync above)
sudo apt-key adv --keyserver hkps://keyserver.ubuntu.com --recv-keys E88979FB9B30ACF2 # google chrome
sudo apt-key adv --keyserver hkps://keyserver.ubuntu.com --recv-keys 1655A0AB68576280 # node source

sudo apt update

# Add something like this to /etc/network/interfaces.d/${interface} replacing wlp9s0 with correct device
#
# auto wlp9s0
# allow-hotplug wlp9s0
#
# iface wlp9s0 inet dhcp
#   wpa-conf /etc/wpa_supplicant.conf

sudo apt update

# system tools
sudo apt install --yes build-essential git gpg vim emacs zsh perl stow ca-certificates wget curl gnupg lsb-release software-properties-common apt-transport-https resolvconf htop bmon net-tools

# Installing resolvconf above causes DNS resolution to fail, so restart networking to fix
# and allow further package installations to work
sudo systemctl restart networking

# networking
sudo apt install --yes firmware-realtek wpasupplicant ifupdown iproute2

# audio
sudo apt install --yes pulseaudio pavucontrol

# gpu
sudo apt install --yes xserver-xorg-video-amdgpu xserver-xorg-video-amdgpu mesa-utils libgl1-mesa-dri firmware-amd-graphics firmware-linux-nonfree

# i3 / windows manager
sudo apt install --yes i3-wm i3status i3lock xorg xterm compton feh rofi scrot xclip terminator polybar

# applications
sudo apt install --yes google-chrome-stable insomnia nodejs docker-ce docker-ce-cli containerd.io clementine

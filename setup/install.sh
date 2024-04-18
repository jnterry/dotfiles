#/usr/bin/bash
# Sets up new debian machine with all required packages

set -e

# sudo apt update

# Ensure third party repos in config are cloned, eg, zsh plugins
git submodule init
git submodule update

# dependencies for this script (and useful to keep on system anyway...)
sudo apt install --yes fontconfig rsync curl

# Copy file structure
sudo rsync -r ./files/* /
/usr/bin/fc-cache -f -v # update font cache (fonts installed by above cp)
mkdir -p /etc/network/interfaces.d

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
sudo apt install --yes build-essential git gpg vim emacs zsh perl stow ca-certificates wget curl gnupg lsb-release software-properties-common apt-transport-https resolvconf htop bmon

# networking
sudo apt install --yes firmware-realtek wpasupplicant ifupdown iproute2

# audio
sudo apt install --yes pulseaudio pavucontrol

# gpu
sudo apt install --yes xserver-xorg-video-amdgpu xserver-xorg-video-amdgpu mesa-utils libgl1-mesa-dri firmware-amd-graphics firmware-linux-nonfree

# i3 build dependencies
# sudo apt install --yes dh-autoreconf libxcb-keysyms1-dev libpango1.0-dev libxcb-util0-dev xcb libxcb1-dev libxcb-icccm4-dev libyajl-dev libev-dev libxcb-xkb-dev libxcb-cursor-dev libxkbcommon-dev libxcb-xinerama0-dev libxkbcommon-x11-dev libstartup-notification0-dev libxcb-randr0-dev libxcb-xrm0 libxcb-xrm-dev libxcb-shape0 libxcb-shape0-dev

# i3 gaps now merged into apt pakcaged i3-wm, so we can just install it directly rather than building i3-gaps from source...

# i3 / windows manager
sudo apt install --yes i3-wm i3status i3lock xorg xterm compton feh rofi scrot xclip terminator polybar

# applications
sudo apt install --yes google-chrome-stable insomnia nodejs docker-ce docker-ce-cli containerd.io clementine

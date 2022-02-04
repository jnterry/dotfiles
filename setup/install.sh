#/usr/bin/bash
# Sets up new debian machine with all required packages

set -e

# Copy file structure
cp -r ./files /
/usr/bin/fc-cache -f -v # update font cache (fonts installed by above cp)
mkdir -p /etc/network/interfaces.d

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
sudo apt install --yes firmwire-realtek wpasupplicant ifupdown iproute2

# audio
sudo apt install --yes pulseaudio pavucontrol

# gpu
sudo apt install --yes xserver-xorg-video-amdgpu xserver-xorg-video-amdgpu mesa-utils libgl1-mesa-dri firmware-amd-graphics firmware-linux-nonfree

# i3 build dependencies
sudo apt install dh-autoreconf libxcb-keysyms1-dev libpango1.0-dev libxcb-util0-dev xcb libxcb1-dev libxcb-icccm4-dev libyajl-dev libev-dev libxcb-xkb-dev libxcb-cursor-dev libxkbcommon-dev libxcb-xinerama0-dev libxkbcommon-x11-dev libstartup-notification0-dev libxcb-randr0-dev libxcb-xrm0 libxcb-xrm-dev libxcb-shape0 libxcb-shape0-dev

# i3 / windows manager
sudo apt install --yes i3status i3lock xorg xterm compton feh rofi scrot xclip terminator polybar

# applications
sudo apt install --yes google-chrome-stable spotify-client insomnia nodejs docker-ce docker-ce-cli containerd.io clementine

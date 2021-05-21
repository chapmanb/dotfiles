# Set of commands to run to bootstrap a new system
#!/bin/bash
set -eu -o pipefail

sudo apt-get update

sudo apt-get install -y gnome-tweak-tool gnome-shell-extensions
sudo apt-get install -y unclutter xdotool xclip xsel dmenu vim-gtk

sudo apt-get install -y git tmux w3m fonts-hack silversearcher-ag ripgrep urlview
sudo apt-get install -y notmuch libnotmuch5 python3-notmuch offlineimap msmtp pass lastpass-cli
sudo apt-get install -y python3-setuptools python3-magic python3-configobj python3-urwid python3-urwidtrees python3-gpg python3-venv python3-wheel python3-dev openjdk-11-jdk-headless

sudo add-apt-repository ppa:kelleyk/emacs
sudo apt-get update
sudo apt install emacs27 gnutls-bin

curl -s https://raw.githubusercontent.com/borkdude/babashka/master/install -o install-babashka && chmod +x install-babashka && ./install-babashka

wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein && chmod +x lein && mv lein /usr/local/bin

python3 -m venv --system-site-packages ~/.mail/env
~/.mail/env/bin/pip install -e 'git+https://github.com/notmuch/notmuch.git#egg=notmuch&subdirectory=bindings/python'
~/.mail/env/bin/pip install 'git+https://github.com/teythoon/afew.git#egg=afew'
~/.mail/env/bin/pip install service_identity
~/.mail/env/bin/pip install 'git+https://github.com/pazz/alot.git#egg=alot'

sudo apt autoremove emacs emacs23 emacs24 emacs25
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt-get update
sudo apt install emacs27 gnutls-bin

# Zoom https://zoom.us/download#client_4meeting

# Google Drive fuse mount
sudo add-apt-repository ppa:alessandro-strada/ppa
sudo apt-get install google-drive-ocamlfuse
google-drive-ocamlfuse
google-drive-ocamlfuse /mnt/drive

## Old stuff

# sudo apt-get install rxvt-unicode

# Windows 10
https://github.com/rupor-github/ssh-agent-wsl

choco install -y docker-desktop emacs gpg4win git hackfont ag hunspell.portable
choco install -y vcxsrv

# Older setup with xmonad
sudo add-apt-repository ppa:gekkio/xmonad
sudo add-apt-repository ppa:nathan-renniewaldock/flux
sudo add-apt-repository ppa:kelleyk/emacs
sudo add-apt-repository ppa:ansible/ansible
sudo add-apt-repository ppa:xapian-backports/ppa
sudo sh -c 'echo "deb https://weechat.org/ubuntu $(lsb_release -cs) main" >> /etc/apt/sources.list.d/weechat.list'
sudo apt-key adv --keyserver keys.gnupg.net --recv-keys 11E9DE8848F2B65222AA75B8D1820DB22A11534E
wget -O - https://pkg.tarsnap.com/tarsnap-deb-packaging-key.asc | sudo apt-key add -
echo "deb http://pkg.tarsnap.com/deb/$(lsb_release -s -c) ./" | sudo tee -a /etc/apt/sources.list.d/tarsnap.list
sudo apt-get install xmonad gnome-session-xmonad 

sudo apt-get install weechat weechat-python weechat-lua weechat-perl weechat-ruby ansible bitlbee python-pip
sudo apt-get install fluxgui
pip install websocket-client
pip install saws

# tarsnap
pip install tarsnapper
sudo apt-get install tarsnap

# docker
curl -fsSL get.docker.com -o get-docker.sh
bash get-docker.sh

git clone -b develop https://github.com/syl20bnr/spacemacs ~/.emacs.d

# notmuch
# mkdir -p ~/install/system
# cd ~/install/system
# wget https://oligarchy.co.uk/xapian/1.4.5/xapian-core-1.4.5.tar.xz
# tar -xJvpf xapian-core-1.4.5.tar.xz
# cd xapian-core-1.4.5/
# ./configure
# make install
# cd ..
# wget https://notmuchmail.org/releases/notmuch-0.26.tar.gz
# tar -xzvpf notmuch-0.26.tar.gz
# cd notmuch-0.26/
# ./configure
# make
# sudo make install
# cd bindings/python
# python setup.py install


# slack -- https://slack.com/downloads/linux
# chrome -- https://www.google.com/chrome/
# citrix -- https://www.citrix.com/downloads/citrix-receiver/linux/
#  https://www.citrix.com/downloads/citrix-receiver/legacy-receiver-for-linux/receiver-for-linux-latest-13-4.html
#  use older citrix
#  https://discussions.citrix.com/topic/385459-ssl-error-with-135-works-with-134/?page=3
cd /opt/Citrix/ICAClient/keystore/caerts
sudo ln -s /usr/share/ca-certificates/mozilla/COMODO_RSA_Certification_Authority.crt

# Startup Applications
# System -> Preferences -> Startup Applications
#Xresources 
xrdb /home/chapmanb/.Xresources
# Syndaemon
# https://ubuntuforums.org/showthread.php?t=2390480
# https://askubuntu.com/questions/1054330/how-can-i-use-my-touchpad-while-typing-in-ubuntu-18-04
syndaemon -i .5 -R -d
# ctrlcaps
setxkbmap -option ctrl:nocaps

# bitlbee -- /var/lib/bitlbee

# Touchpad fixes
# https://askubuntu.com/questions/483707/14-04-touchpad-is-too-sensitive
 # Does not seem to help, syndaemon above
#xinput set-prop "SynPS/2 Synaptics TouchPad" "Synaptics Noise Cancellation" 10 10
#xinput set-prop "SynPS/2 Synaptics TouchPad" "Synaptics Finger" 50 70 257


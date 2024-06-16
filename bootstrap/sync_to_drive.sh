#!/bin/bash
set -eu -o pipefail
# rclone copy ~/personal gdrive:personal
# rclone copy ~/ginkgo/reviews gdrive:ginkgo/reviews

for TOMOVE in .aws .bcbio .bashrc.local .bashrc.ginkgo .gnupg .ec2 .keybase .lein .password-store .pypirc .ssh .tarsnap .vpn .vim .offlineimap .fzf.bash .fzf; do
	rclone copy -v ~/$TOMOVE gdrive:dotfiles/$TOMOVE
done

for TOMOVE in nvim msmtp gspread; do
	rclone copy -v ~/.config/$TOMOVE gdrive:dotfiles/.config/$TOMOVE
done

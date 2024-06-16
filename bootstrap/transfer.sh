#!/bin/bash
set -eu -o pipefail

ORIGIP=192.168.180.170
USER=bchapman
# for TOMOVE in .aws .bcbio .bashrc.local .bashrc.ginkgo .gnupg .ec2 .keybase .lein .password-store .pypirc .ssh .tarsnap .vpn .vim .offlineimap .fzf.bash .fzf
# do
# 	rsync -rav $USER@$ORIGIP:~/$TOMOVE ~/
# done

#rsync -ravz $USER@$ORIGIP:personal ~/
#rsync -ravz $USER@$ORIGIP:servers ~/
#rsync -rav $USER@$ORIGIP:.tmux/resurrect ~/.tmux/
#rsync -rav $USER@$ORIGIP:.config/msmtp ~/.config/
#rsync -rav $USER@$ORIGIP:.config/gspread ~/.config/
#rsync -ravz $USER@$ORIGIP:ginkgo ~/
#rsync -ravz $USER@$ORIGIP:/mnt/work/ginkgo /mnt/work
#rsync -rav $USER@$ORIGIP:mail ~/
rsync -rav $USER@$ORIGIP:install/system ~/install
rsync -rav $USER@$ORIGIP:install/clojure ~/install


INDIR=~/transfer
# for TOMOVE in `ls -a1d $INDIR/.*`
# do
# 	echo $TOMOVE "~/`basename $TOMOVE`"
# 	rsync -rav $TOMOVE ~/`basename $TOMOVE`
# done
#rsync -rav $INDIR/mail/fastmail ~/mail/
#rsync -rav $INDIR/mail/ginkgo ~/mail/
#rsync -rav $INDIR/mail/.notmuch ~/mail/
#rsync -rav $INDIR/personal ~/
#rsync -rav $INDIR/servers ~/

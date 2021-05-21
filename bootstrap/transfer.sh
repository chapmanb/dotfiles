#!/bin/bash
set -eu -o pipefail

ORIGIP=192.168.180.138
USER=bchapman
for TOMOVE in .aws .bcbio .bashrc.local .bashrc.ginkgo .gnupg .ec2 .keybase .lein .password-store .pypirc .ssh .tarsnap .vpn .vim .offlineimap
do
	rsync -rav $USER@$ORIGIP:~/$TOMOVE ~/
done

#rsync -rav $USER@$ORIGIP:personal ~/
#rsync -rav $USER@$ORIGIP:servers ~/
#rsync -rav $USER@$ORIGIP:.tmux/resurrect ~/.tmux/
#rsync -rav $USER@$ORIGIP:.config/msmtp ~/.config/
#rsync -rav $USER@$ORIGIP:ginkgo ~/
#rsync -rav $USER@$ORIGIP:mail ~/


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

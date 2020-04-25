#!/bin/bash
set -eu -o pipefail

ORIGIP=192.168.11.24
#for TOMOVE in .aws .bashrc.local .ec2 .gnupg .keybase .lein .password-store .pypirc .ssh .tarsnap .vpn .weechat
for TOMOVE in .aws .bcbio .bashrc.local .ec2 .keybase .password-store .pypirc .ssh .tarsnap .vpn
do
	rsync -rav chapmanb@$ORIGIP:~/$TOMOVE ~/
done
#rsync -rav chapmanb@$ORIGIP:mail ~/
#rsync -rav chapmanb@$ORIGIP:docs ~/
rsync -rav chapmanb@$ORIGIP:personal ~/
#rsync -rav chapmanb@$ORIGIP:drive ~/

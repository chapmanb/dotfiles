#!/bin/bash
set -eu -o pipefail
rclone sync drive:mail ~/drive/mail
rclone sync drive:org ~/drive/org

#!/bin/sh
notmuch search --output=files tag:deleted | xargs -l rm -f

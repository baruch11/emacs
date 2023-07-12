#!/bin/bash

# customize these
WGET=/opt/homebrew/bin/wget
ICS2ORG=~/.emacs.d/scripts/ical2org.awk
ICSFILE=~/.emacs.d/bouboulinos.ics
ORGFILE=~/.emacs.d/bouboulinos.org
# no customization needed below

$WGET -O $ICSFILE $GCAL_PRIVATE_ADDRESS
$ICS2ORG < $ICSFILE > $ORGFILE

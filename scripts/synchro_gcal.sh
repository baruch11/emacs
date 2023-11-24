#!/bin/bash

# customize these
WGET=/opt/homebrew/bin/wget
ICS2ORG=ical2orgpy
ICSFILE=~/.emacs.d/bouboulinos.ics
ORGFILE=~/.emacs.d/bouboulinos.org
# no customization needed below

export CALENDAR="perso"
$WGET -O $ICSFILE $GCAL_PRIVATE_ADDRESS
$ICS2ORG  $ICSFILE  $ORGFILE

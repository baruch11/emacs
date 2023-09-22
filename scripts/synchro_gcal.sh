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

export CALENDAR="QM"
$WGET -O ~/.emacs.d/outlook_qm.ics $QMCALICS
$ICS2ORG   ~/.emacs.d/outlook_qm.ics  ~/.emacs.d/outlook_qm.org

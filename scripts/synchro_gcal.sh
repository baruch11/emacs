#!/bin/bash

# customize these
WGET=/usr/local/bin/wget
ICS2ORG=/Users/charlesprat/.local/bin/ical2org.awk
ICSFILE=/Users/charlesprat/.emacs.d/bouboulinos.ics
ORGFILE=/Users/charlesprat/.emacs.d/bouboulinos.org
# no customization needed below

$WGET -O $ICSFILE $GCAL_PRIVATE_ADDRESS
$ICS2ORG < $ICSFILE > $ORGFILE

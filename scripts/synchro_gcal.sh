#!/bin/bash

# customize these
WGET=/usr/local/bin/wget
ICS2ORG=/Users/charlesprat/.local/bin/ical2org.awk
ICSFILE=/Users/charlesprat/.emacs.d/bouboulinos.ics
ORGFILE=/Users/charlesprat/.emacs.d/bouboulinos.org
URL=my private google cal address

# no customization needed below

$WGET -O $ICSFILE $URL
$ICS2ORG < $ICSFILE > $ORGFILE

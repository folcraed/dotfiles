#!/usr/bin/sh

# Simple little script to pipe cmus song names to i3blocks.
# Works for local files or music streams from internet
# as long as they provide song name in standard way.

s=$(cmus-remote -Q | grep stream | tail -1 | cut -c 7-)
    if [ "$s" == "" ]
        then
        s=$(cmus-remote -Q | grep title | cut -c 10-)
   fi
echo $s

#!/bin/bash

for appdir in \
    $HOME/.Trash/Rhythmr/Rhythmr.app \
    $HOME/rhythmr/Rhythmr/Rhythmr.app \
    $HOME/Desktop/Rhythmr/Rhythmr.app \
    ; do
  echo removing $appdir
  /System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -u "$appdir"
done

/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -u Rhythmr.app/
/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -u build/app-template/Rhythmr.app
/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -R -f -u /Users/gmt/rhythmr/build/app-template/Rhythmr.app
/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -R -f -u /Users/gmt/rhythmr/build/app-template/Rhythmr.app-template
echo remaining registrations:
/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -dump | grep -i rhythmr | grep path:

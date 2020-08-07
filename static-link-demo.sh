#!/bin/bash

set -x

rm -f StaticLinkDemo src/StaticLinkDemo.o src/*.hi src/StaticLinkDemo.o
mkdir static-libs

gcc -o src/nonblocking.o -c src/nonblocking.c

# These obtained via:
#   $ otool -l /usr/local/opt/portaudio/lib/libportaudio.2.dylib | grep -i name
ln -s /System/Library/Frameworks/CoreAudio.framework/Versions/A/CoreAudio static-libs/CoreAudio.a
ln -s /System/Library/Frameworks/AudioToolbox.framework/Versions/A/AudioToolbox static-libs/AudioToolbox.a
ln -s /System/Library/Frameworks/AudioUnit.framework/Versions/A/AudioUnit static-libs/AudioUnit.a
ln -s /System/Library/Frameworks/Carbon.framework/Versions/A/Carbon static-libs/Carbon.a
ln -s /System/Library/Frameworks/CoreFoundation.framework/Versions/A/CoreFoundation static-libs/CoreFoundation.a
ln -s /System/Library/Frameworks/CoreServices.framework/Versions/A/CoreServices static-libs/CoreServices.a
(ghc -o StaticLinkDemo src/StaticLinkDemo.hs src/nonblocking.o -L./static-libs/ /usr/local/Cellar/portaudio/19.6.0/lib/libportaudio.a \
  static-libs/AudioToolbox.a static-libs/AudioUnit.a static-libs/Carbon.a static-libs/CoreAudio.a static-libs/CoreFoundation.a static-libs/CoreServices.a) 2>&1 | tee out

rm -r static-libs
rm src/*.hi
rm src/*.o

./StaticLinkDemo

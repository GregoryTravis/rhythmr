#./static-link-demo.sh
#exit

#stack exec -- ghc-pkg unregister portaudio
#(stack build --verbose --force-dirty rhythmr) 2>&1 | tee out
#exit

# ./build/reinstall-ls
# exit

# separate music & rhythm
# rm -f out?.wav
# python audiolabs-erlangen.py

# Construct a song
# rm song.wav loop?.wav
#(stack build --ghc-options=-v3 --force-dirty rhythmr && DYLD_PRINT_LIBRARIES=YES stack exec rhythmr -- "$@") 2>&1 | tee out

#rm -r swingin.rhythmr/loops/swingin-*blossom/
#rm swingin.rhythmr/loops/swingin-bass-blossom/loop-download-6ac4d5295ae338b1e943ccf88d67537c-18386e8930f70cdc57af9c000ca46d27.wav
(stack build rhythmr && stack exec rhythmr -- "$@") 2>&1 | tee out
#p swingin.rhythmr/loops/swingin-bass-blossom/loop-download-6ac4d5295ae338b1e943ccf88d67537c-18386e8930f70cdc57af9c000ca46d27.wav
#ls -lt swingin.rhythmr/loops/swingin-bass-blossom/

# profargs='--library-profiling --executable-profiling --profile'
# (stack build --force-dirty $profargs --ghc-options="-fprof-auto -O0" rhythmr && stack exec rhythmr $profargs -- "$@" +RTS -pa -xc -RTS) 2>&1 | tee out

stty sane

# Download mp3s
# python get-mp3s.py | tee out.json

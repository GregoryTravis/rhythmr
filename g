# cp distribution/app-template/Rhythmr.app/Contents/MacOS/Rhythmr Rhythmr.app/Contents/MacOS/
# exit

# separate music & rhythm
# rm -f out?.wav
# python audiolabs-erlangen.py

# Construct a song
# rm song.wav loop?.wav
(stack build rhythmr && stack exec rhythmr -- "$@") 2>&1 | tee out

# profargs="--library-profiling --executable-profiling --profile"
# (stack build $profargs rhythmr && stack exec rhythmr $profargs -- "$@" +RTS -xc -RTS) 2>&1 | tee out

stty sane

# Download mp3s
# python get-mp3s.py | tee out.json

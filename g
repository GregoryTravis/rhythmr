# separate music & rhythm
# rm -f out?.wav
# python audiolabs-erlangen.py

# Construct a song
# rm song.wav loop?.wav
(stack build rhythmr && stack exec rhythmr -- $*) 2>&1 | tee out
#(stack build --library-profiling --executable-profiling --profile rhythmr) 2>&1 | tee out

stty sane

# Download mp3s
# python get-mp3s.py | tee out.json

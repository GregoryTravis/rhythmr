#rm -f out?.wav
#python audiolabs-erlangen.py
rm song.wav loop?.wav
(stack build autobeat && stack exec autobeat) 2>&1 | tee out

#rm -f out?.wav
#python audiolabs-erlangen.py
rm snip.wav
(stack build autobeat && stack exec autobeat) 2>&1 | tee out

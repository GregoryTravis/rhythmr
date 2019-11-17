#rm -f out?.wav
#python audiolabs-erlangen.py
(stack build autobeat && stack exec autobeat) 2>&1 | tee out

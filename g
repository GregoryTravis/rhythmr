#rm -f out?.wav
#python audiolabs-erlangen.py
(stack build y && stack exec y) 2>&1 | tee out

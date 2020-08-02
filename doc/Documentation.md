Documentation
======

** Overview 

When using Rhythmr, you are presented with a set of loops, playing in unison. If you like what you hear, hit the right arrow, and you'll get another one to listen to. If you don't like it, hit the left arrow.

As you do this, good-sounding combinations of the loops appear in the upper right. Once you have a few, you can generate a song from them by hitting "S" or "J". If you like the song, write it to disk using "W".

When you start up, a pool of loops is selected, shown in the upper left. This is often only a subset of the loops contained in the project. When you get bored with these, hit "E" ("new pool") to load a new set. (The ones you've already liked will stay.)

"Cycle affinities" (the "c" key) generates another song using the same groups of loops, but in different combinations. Sometimes it sounds very similar, sometimes very different.

** User interface

Rhythmr only uses the keyboard -- no mouse or menus.

Details at the bottom.

* **ESC** - save and quit
* **Control-S** - save (but don't quit)
* **Control-Q** - quit (if you've already saved)
* **Shift-Control-Q** - quit without saving
* **u** - undo
* **Control-R** - redo
* **Shift-Control-u** - undo all the way (complete reset)
* **Shift-Control-R** - redo all the way (latest state)
* **c** - cycle affinities
* **right arrow** - like
* **left arrow** - dislike
* **r, space** - new group, randomly
* **i** - new group, incrementally
* **E** - new pool
* **W** - write current song to disk
* **S** - generate song
* **J** - generate drum-n-bass-style song

Deprecated commands:
* **s** - new group, subsets
* **d** - new group, divide-and-conquer
* **A** - make the first affinity the current group

** Running at the command-line

# Go into the Rhythmr folder

$ cd Rhythmr/

# You'll see the app and two Rhythmr projects

$ ls
60smotown.rhythmr/	Rhythmr.app/		koop-it-again.rhythmr/

# Run the executable directly, with no arguments. It prints out help info

$ Rhythmr.app/Contents/MacOS/rhythmr
Portuadio starting
"++ rhythmr []"
rhythmr command project-dir [arg, arg, arg, ...]

Commands include:

rhythmr barsSearch project-dir collection-name search-string num-tracks
rhythmr barsId project-dir collection id
rhythmr barsIdFile project-dir collection filename [filename, filename, ...]
rhythmr barsFile project-dir collection filename [filename, filename, ...]
rhythmr aff project-dir collection weight [collection weight, ...]
rhythmr credits

Portuadio shutdown
Test finished.

# Run one of the projects

$ Rhythmr.app/Contents/MacOS/rhythmr aff koop-it-again.rhythmr/

Documentation
======

## TL;DR

* Swipe left and right on each set of loops (use arrow keys)
* Generate a song with one of: S, T, H, L, G, J, A

## Overview

When using Rhythmr, you are presented with a set of loops, playing in unison. If you like what you hear, hit the right arrow, and you'll get another one to listen to. If you don't like it, hit the left arrow.

As you do this, good-sounding combinations of the loops appear in the upper right. Once you have a few, you can generate a song from them by hitting "S" or "J". If you like the song, write it to disk using "W".

When you start up, a pool of loops is selected, shown in the upper left. This is often only a subset of the loops contained in the project. When you get bored with these, hit "E" ("new pool") to load a new set. (The ones you've already liked will stay.)

"Cycle affinities" (the "c" key) causes the next song to shift the loop groups, so that re-generating a song will produce a different song. Repeatedly pressing c will repeatedly shift the groups.

## Backwards Compatibility

Rhythmr is in constant flux, so don't expect your save files to work in subsequent versions.

## User interface

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
* **S, T, H, L, G, J, A** - generate song from a template (see below)
* **J** - generate drum-n-bass-style song
* **A** - generate even more messed up drum-n-bass-style song
* **0-9** - volume

Deprecated commands:
* **s** - new group, subsets
* **d** - new group, divide-and-conquer
* **A** - make the first affinity the current group

## Song templates

There are seven song templates, identified by the keyboard shortcut and inscrutable name:

#### S (cycleLikesSong)

Chops up your likes and brings parts of them in and out. Tries multiple
different orderings of the loops in a like.

#### T (tallSong)

Like S, but only uses likes of size 4 or greater. Does not try multiple
orderings.

#### H (thresholdSong)

Only uses your likes, verbatim, does not create its own variations and
reductions of them. Sequences them so that adjacent ones are as similar as
possible. **Warning** this one is slow, so if this key just hangs the program,
try again with fewer likes.

#### L (likesSong)

Just your likes, in the order you liked them.

#### G (metagraphSong)

Like H, but not slow! More random combinations of similar sounds, without
guaranteeing maximal similarity. Good enough.

#### J (chew)

Sorta drum+bass. Takes one of the loops and chops bits out and moves them to
the end, producing some very intentional-sounding variation.

#### A (hiChew)

Like J, but more zany.

## Running at the command-line

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

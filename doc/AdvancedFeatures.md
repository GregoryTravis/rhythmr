Advanced Features
======

## Advanced Features

Rhythmr can extract loops from YouTube content, either via URL, YouTube ID, or search term.

For this to work, you must install some dependencies, and run Rhythmr at the command-line.

### Install brew (optional but recommended)

brew is a command-line utility that makes installing and uninstalling other packages easy.

[Instructions are here](https://brew.sh/)

### Install youtube-dl

*If using Brew:*

```
$ brew install youtube-dl
```

*If not using Brew:*

[Instructions are here](https://ytdl-org.github.io/youtube-dl/download.html)

### Install Sonic Annotator

[Download the binary](https://code.soundsoftware.ac.uk/projects/sonic-annotator/files) and place it in a 'bin' subdirectory under your current working directory.

```
# (Assuming you have downloaded the tar.gz to your ~/Downloads)
$ cd my-working-dir
$ tar -xzvf ~/Downloads/sonic-annotator-1.6.macos.tar.gz
$ mkdir bin
$ mv sonic-annotator-1.6-macos/sonic-annotator bin/sonic-annotator 
$ rm -r sonic-annotator-1.6-macos
```

### Install the VAMP QM (Queen Mary, University of London) Plugins

1. [Download the qm-vamp-plugins package](https://code.soundsoftware.ac.uk/projects/qm-vamp-plugins/files)
2. Unpack the archive
3. Follow the instructions in INSTALL.txt

### Run Rhythmr at the command line

Assuming that the Rhythmr app is on your Desktop, you can run the binary like this:

```
$ ~/Desktop/Rhythmr.app/Contents/MacOS/rhythmr
```

(You are also free to copy the binary to a more convenient location; it doesn't need to be inside the app.)

If that worked, you'll see output like this:

```
Portuadio starting
"++ rhythmr []"
rhythmr command project-dir [arg, arg, arg, ...]

Commands include:

rhythmr barsSearch project-dir collection-name search-string num-tracks
rhythmr barsId project-dir collection id
rhythmr barsIdFile project-dir collection filename [filename, filename, ...]
rhythmr barsFile project-dir collection filename [filename, filename, ...]
rhythmr aff project-dir collection weight [collection weight, ...]
rhythmr demo project-dir collection weight [collection weight, ...]
rhythmr credits

Portuadio shutdown
Test finished.
```

This shows the available commands. To search youtube and create a new Rhythmr project from the results:

```
$ ~/Desktop/Rhythmr.app/Contents/MacOS/rhythmr barsSearch my-new-project.rhythmr paula-abdul "paula.abdul" 4
```

This command says the following:

* Create or reuse a project called "my-new-project"
* Create or reuse a collection inside the project called "paula-abdul"
* Seach YouTube for "paul abdul"
* Download four tracks

(**Note:** you must put periods between the words of the search term, for reasons I am too embarrassed to admit.)

Once this is done, you can use my-new-project.rhythmr just like any Rhythmr project.

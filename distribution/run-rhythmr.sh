# This script is inserted into the app using Automator.
date >> /Users/gmt/rhythmr/boo
echo args $* >> /Users/gmt/rhythmr/boo
/Users/gmt/rhythmr/Rhythmr.app/Contents/MacOS/rhythmr-e aff /Users/gmt/rhythmr/koop-it-again.rhythmr/ >> boo
date >> /Users/gmt/rhythmr/boo
echo args $* >> /Users/gmt/rhy

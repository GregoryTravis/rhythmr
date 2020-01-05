module Display (displayMain) where

import Graphics.Gloss

displayMain = display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)

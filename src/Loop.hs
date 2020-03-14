module Loop (Loop(..)) where

-- data Loop = Loop String Sound

-- instance Eq Loop where
--   (Loop s _) == (Loop s' _) = s == s'

-- instance Show Loop where
--   show (Loop s _) = show s

-- instance Ord Loop where
--   (Loop s _) <= (Loop s' _) = s <= s'

newtype Loop = Loop String
  deriving (Eq, Read, Show, Ord)

-- instance Eq Loop where
--   (Loop s) == (Loop s') = s == s'

-- instance Show Loop where
--   show (Loop s) = show s

-- instance Ord Loop where
--   (Loop s) <= (Loop s') = s <= s'

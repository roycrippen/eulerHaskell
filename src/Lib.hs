module Lib where

import           P001_020                       ( solutionsP001_020 )
import           P021_040                       ( solutionsP021_040 )

solutions :: [IO ()]
solutions = solutionsP001_020 ++ solutionsP021_040

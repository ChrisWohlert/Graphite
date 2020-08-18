module Types where

import Data.Text hiding (map, concatMap, zipWith, foldl)
import Reanimate
import Reanimate.Builtin.Documentation
import Reanimate.Animation
import Control.Monad.Reader

data Bar = Bar { barName :: Text, barValue :: Double }

data BarChartSetting = BarChartSetting { barSetting :: BarSetting, colorPalette :: [String] }
data BarSetting = BarSetting { barSignal :: Signal }

data Graph = BarChart { bars :: [Bar], barChartSettings :: BarChartSetting }
           | LineChart
{-# LANGUAGE OverloadedStrings #-}


module Main where


import Reanimate
import Reanimate.Builtin.Documentation
import Reanimate.Animation
import Debug.Trace
import Codec.Picture.Types
import Control.Monad.Reader

import GraphAnimation
import Graphics.SvgTree.Types hiding (Text)
import Data.Text hiding (map, concatMap, zipWith, foldl)
import Data.Foldable
import Types

main :: IO ()
main = reanimate $ rootEnv $ setDuration 4 $ animateGraph (BarChart [Bar "Name" 6, Bar "Name" 2, Bar "Name" 4] (BarChartSetting (BarSetting (curveS 5)) ["blue", "green"]))
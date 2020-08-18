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

main :: IO ()
main = reanimate $ rootEnv $ setDuration 4 $ sceneAnimation barScene
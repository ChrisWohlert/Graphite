{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module GraphAnimation where


import Reanimate
import Reanimate.Builtin.Documentation
import Reanimate.Animation
import Debug.Trace
import Codec.Picture.Types
import Control.Monad.Reader

import Graphics.SvgTree.Types hiding (Text)
import Data.Text hiding (map, concatMap, zipWith, foldl)
import Data.Foldable
import Types 
import AnimEnv


instance Semigroup Tree where
  a <> b = mkGroup [b, a]

animateGraph :: Graph -> Animation
animateGraph g = sceneAnimation $ mkScene g




rootEnv = mapA $ \svg -> mkGroup
  [ mkBackground "white"
  , withFillOpacity 0 $
    withStrokeWidth 0.1 $
    withStrokeColor "black" (mkGroup [svg]) ]


grid = mkGroup . map ( withStrokeColorPixel (PixelRGBA8 100 100 100 150)
                     . withStrokeWidth 0.01) $ rowLines ++ colLines

rowLines = map (\x -> mkLine (-8, x) (8, x)) [bottom, (bottom / 1.1) .. 5.5] 

colLines = map (\x -> mkLine (x, -5) (x, 5)) [-8, -7.5 .. 8]

mkScene :: Graph -> Scene s ()
mkScene b@(BarChart bars settings) = do
  fork $ play $ animate $ const grid
  fork $ play $ animate $ const bottomLeftAxis
  fork $ animateBars settings bars 

mkScene _ = undefined

animateBars setting bars = mkSceneGroup $ zipWith ((\ sprite i -> do
     s <- sprite
     spriteMap s (\ svg -> svg # translate i 0)) . animateBar setting) bars [0, 1.1 ..]

animateBar s (Bar name value) = 
    newSprite $ (\t -> mkRect 1 (value * t)
                            # withStrokeWidth 0.03
                            # translate 0 (value * t / 2)
                          <>
                            mkText name 
                              # scale 0.2 
                              # rotate 310 
                              # translate 0.1 (-1)) . (barSignal . barSetting $ s)
                          <$>
                            spriteT

bottomLeftAxis = mkLine (bottomLeft # moveUp 2 # moveRight 1) (topLeft # moveRight 1) <> mkLine (bottomLeft # moveUp 2 # moveRight 1) (bottomRight # moveUp 2)

mkSceneGroup = foldl fork' (wait 0)

fork' a b = do
  fork a
  fork b

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
 



instance Semigroup Tree where
  a <> b = mkGroup [b, a]
   
rootEnv = mapA $ \svg -> mkGroup
  [ mkBackground "white"
  , withFillOpacity 0 $
    withStrokeWidth 0.1 $
    withStrokeColor "black" (mkGroup [svg]) ]


grid = mkGroup . map ( withStrokeColorPixel (PixelRGBA8 100 100 100 150)
                     . withStrokeWidth 0.01) $ rowLines ++ colLines

rowLines = map (\x -> mkLine (-8, x) (8, x)) [bottom, (bottom / 1.1) .. 5.5] 

colLines = map (\x -> mkLine (x, -5) (x, 5)) [-8, -7.5 .. 8]

barScene :: Scene s ()
barScene = do
  fork $ play $ animate $ const bottomLeftAxis
  fork $ play $ animate $ const grid
  fork $ toScene $ BarChart [Bar "Name" 6, Bar "Name" 2, Bar "Name" 4]

bottomLeftAxis = mkLine (bottomLeft # moveUp 2 # moveRight 1) (topLeft # moveRight 1) <> mkLine (bottomLeft # moveUp 2 # moveRight 1) (bottomRight # moveUp 2)

top = 4.5
bottom = -4.5
left = -8
right = 8
topLeft = (left, top)
topRight = (right, top)
bottomLeft = (left, bottom)
bottomRight = (right, bottom)
height = abs bottom + top
width = abs left + right

moveUp v (a, b) = (a, b + v)
moveRight v (a, b) = (a + v, b)

smallMargin = 0.5
mediumMargin = 1.5


data Bar = Bar { barName :: Text, barValue :: Double }

data BarChart = BarChart { bars :: [Bar]
                         }


class ToScene a where
  toScene :: a -> Scene s ()

instance ToScene BarChart where
  toScene (BarChart bars) = mkSceneGroup $ zipWith ((\ sprite i -> do
     s <- sprite
     spriteMap s (\ svg -> svg # translate i 0)) . toSprite) bars [0, 1.1 ..]


class ToSprite a where
  toSprite :: a -> Scene s (Sprite s)

instance ToSprite Bar where
  toSprite (Bar name value) =
    newSprite $ (\t -> mkRect 1 (value * t)
                            # withStrokeWidth 0.03
                            # translate 0 (value * t / 2)
                          <>
                            mkText name 
                              # scale 0.2 
                              # rotate 310 
                              # translate 0.1 (-1)) . curveS 5
                          <$>
                            spriteT


mkSceneGroup = foldl fork' (wait 0)

fork' a b = do
  fork a
  fork b
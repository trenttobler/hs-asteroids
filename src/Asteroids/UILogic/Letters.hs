module Asteroids.UILogic.Letters (
  LetterScale, letterScaleX, letterScaleY

  , DrawableLetters, isDrawableLetter, drawLetter, letterScale, letterScreen
  , letterGridPt2, letterSizePt2

  , LetterShapes, toDrawableLetters
  , LetterShape, toDrawableLetter
  , letterShapes

  , drawText, drawCenteredText
  ) where

import           Asteroids.UILogic.Drawable
import           Data.Char                  (toUpper)
import           Data.HashMap.Lazy          as H
import           Data.List                  (intersperse)
import           Data.Maybe                 (fromMaybe)

type LetterScale = (Float,Float)
letterScaleX, letterScaleY :: LetterScale -> Float
letterScaleX (dx,_) = dx
letterScaleY (_,dy) = dy

type LetterShape = [[(Coord,Coord)]]
newtype LetterShapes = LetterShapes (H.HashMap Char LetterShape)
newtype DrawableLetters = DrawableLetters ((Int,Int), LetterScale, H.HashMap Char (IO ()))

letterLookup :: DrawableLetters -> H.HashMap Char (IO ())
letterLookup (DrawableLetters (_,_,h)) = h

letterScale :: DrawableLetters -> LetterScale
letterScale (DrawableLetters (_,x,_)) = x

letterScreen :: DrawableLetters -> (Int,Int)
letterScreen (DrawableLetters (x,_,_)) = x

toDrawableLetter :: LetterShape -> (Int,Int) -> LetterScale -> IO ()
toDrawableLetter shape sizeS sizeC = let
  toPoly = pt2ToPoly . fmap Pt2
  drawAll = fmap (drawLine . toPoly)
  shape' = resizeLetter sizeS sizeC shape
  in do
    sequence_ $ drawAll shape'
    return ()

letterGridPt2 :: DrawableLetters -> (Int,Int) -> Pt2 Coord
letterGridPt2 h (r,c) = pt2 x y
  where (mr,mc) = letterScreen h
        x = 2 * ( 0.5 + fromIntegral c ) / fromIntegral mc - 1
        y = 1 - 2 * ( 0.5 + fromIntegral r ) / fromIntegral mr

letterSizePt2 :: DrawableLetters -> Pt2 Coord
letterSizePt2 h = pt2 dx dy
  where (mr,mc) = letterScreen h
        dx = 2 / fromIntegral mc
        dy = 2 / fromIntegral mr

resizeLetter :: (Int,Int) -> LetterScale -> LetterShape -> LetterShape
resizeLetter (rows,cols) (dx,dy) = fmap fromL
  where fromP (x,y) = ( dx * x / fromIntegral cols
                      , dy * y / fromIntegral rows )
        fromL = fmap fromP

toDrawableLetters :: LetterShapes -> (Int,Int) -> LetterScale -> DrawableLetters
toDrawableLetters (LetterShapes ss) sizeS sizeC = DrawableLetters letters'
  where letters' = (sizeS, sizeC, toLetters')
        addLetter h k v = H.insert k (toDrawableLetter v sizeS sizeC) h
        toLetters' = H.foldlWithKey' addLetter H.empty ss

isDrawableLetter :: Char -> DrawableLetters -> Bool
isDrawableLetter c h = member (toUpper c) (letterLookup h)

drawLetter :: DrawableLetters -> Char -> IO ()
drawLetter h ch = exactC'
  where exactC' = fromMaybe upperC' $ H.lookup ch (letterLookup h)
        upperC' = fromMaybe (return ()) $ H.lookup (toUpper ch) (letterLookup h)

drawCenteredText :: DrawableLetters -> Pt2 Coord -> [String] -> IO ()
drawCenteredText letters pos s = let
  sizeC = letterSizePt2 letters
  posNextC' = moveTo $ sizeC * pt2 1 0
  posNextL' = moveTo $ sizeC * pt2 0 (-1)
  drawC' = drawLetter letters
  drawL' line = innerDrawing $ do
    moveTo ( sizeC * pt2 (-0.5 * fromIntegral (length line)) 0  )
    sequence_ $ intersperse posNextC' $ fmap drawC' line
  drawS' text = sequence_ $ intersperse posNextL'
                          $ fmap drawL' text
  draw' = innerDrawing $ do
    moveTo pos
    drawS' s
  in draw'

drawText :: DrawableLetters -> Pt2 Coord -> [String] -> IO ()
drawText letters pos s = let
  sizeC = letterSizePt2 letters
  posNextC' = moveTo $ sizeC * pt2 1 0
  posNextL' = moveTo $ sizeC * pt2 0 1
  drawC' = drawLetter letters
  drawL' line = innerDrawing $ sequence_
                             $ intersperse posNextC'
                             $ fmap drawC' line
  drawS' text = sequence_ $ intersperse posNextL'
                          $ fmap drawL' text
  draw' = innerDrawing $ do
    moveTo pos
    drawS' s
  in draw'

letterShapes :: LetterShapes
letterData :: [(Char,[[(Coord, Coord)]])]
letterShapes = LetterShapes $ H.fromList letterData
letterData =
  [ ( 'A', [ [ (-1,-1), (-1, 0), (0, 1), (1, 0), (1,-1) ]
             , [ (-1, 0), (1, 0) ] ] )
  , ( 'B', [ [ (-1, -1), (-1, 1), (0.5, 1), (1, 0.75), (1, 0.25)
             , (0.5,0), (-1,0) ]
           , [ (-1, -1), (0.5,-1), (1,-0.75), (1,-0.25), (0.5,0) ] ] )
  , ( 'C', [ [ ( 1,  0.5), ( 0.5,  1), (-0.5,  1), (-1, 0.5)
             , (-1, -0.5), (-0.5, -1), ( 0.5, -1), (1, -0.5) ] ] )
  , ( 'D', [ [ (-1, -1), (-1, 1), (0, 1), (1, 0.5)
             , (1,-0.5), (0,-1), (-1,-1) ] ] )
  , ( 'E', [ [ (-1,0), (0,0) ]
           , [ (1,-1), (-1,-1), (-1,1), (1,1) ] ] )
  , ( 'F', [ [ (-1,-1), (-1,1), (1,1) ]
           , [ (-1,0), (0,0) ] ] )
  , ( 'G', [ [ ( 1,  0.5), ( 0.5,  1), (-0.5,  1), (-1, 0.5)
             , (-1, -0.5), (-0.5, -1), ( 0.5, -1), (1, -0.5)
             , (1, 0), (0, 0) ] ] )
  , ( 'H', [ [ (-1,-1), (-1,1) ]
           , [ (1,-1), (1,1) ]
           , [ (-1,0), (1,0) ] ] )
  , ( 'I', [ [ (0,-1), (0,1) ]
           , [ (-1,1), (1,1) ]
           , [ (-1,-1), (1,-1) ] ] )
  , ( 'J', [ [ (-1, -0.5), (-0.5, -1), (0.5, -1), (1, -0.5), ( 1, 1 ) ] ] )
  , ( 'K', [ [ (-1, -1), (-1, 1) ]
           , [ (1, 1), (-1, 0), (1, -1)] ] )
  , ( 'L', [ [ (-1, 1), (-1, -1), (1, -1) ] ] )
  , ( 'M', [ [ (-1, -1), (-1, 1), (0, 0), (1, 1), (1, -1)] ] )
  , ( 'N', [ [ (-1, -1), (-1, 1), (1, -1), (1, 1)] ] )
  , ( 'O', [ [ (-1, -0.5), (-0.5, -1), ( 0.5, -1), (1, -0.5)
             , ( 1,  0.5), ( 0.5,  1), (-0.5,  1), (-1, 0.5)
             , (-1, -0.5) ] ] )
  , ( 'P', [ [ (-1, -1), (-1, 1), (0.5, 1), (1, 0.75), (1, 0.25)
             , (0.5,0), (-1,0) ] ] )
  , ( 'Q', [ [ (-1, -0.5), (-0.5, -1), ( 0,   -1), ( 1, 0  )
             , ( 1,  0.5), ( 0.5,  1), (-0.5,  1), (-1, 0.5)
             , (-1, -0.5) ]
           , [ (0, 0), (1, -1) ] ] )
  , ( 'R', [ [ (-1, -1), (-1, 1), (0.5, 1), (1, 0.75), (1, 0.25)
             , (0.5, 0), (-1, 0) ]
           , [ (-0.25, 0), (1, -1) ] ] )
  , ( 'S', [ [ ( 1,  0.5), ( 0.5,  1), (-0.5,  1), (-1, 0.75), (-1, 0.5)
             , ( -0.5, 0), (0.5, 0)
             , ( 1, -0.5), (1, -0.75), (0.5, -1), (-0.5, -1), (-1, -0.5) ] ] )
  , ( 'T', [ [ ( 0, -1), ( 0, 1 ) ]
           , [ (-1, 1), (1, 1) ] ] )
  , ( 'U', [ [ (-1, 1), (-1, -0.5), (-0.5, -1), ( 0.5, -1)
             , (1, -0.5), (1, 1 ) ] ] )
  , ( 'V', [ [ (-1, 1), (0, -1), (1, 1) ] ] )
  , ( 'W', [ [ (-1, 1), (-1, -1), (0, 0), (1, -1), (1, 1) ] ] )
  , ( 'X', [ [ (-1, -1), (1, 1) ], [ (1, -1), (-1, 1) ] ] )
  , ( 'Y', [ [ (-1, 1), (0, 0) ], [ (1, 1), (-1, -1) ] ] )
  , ( 'Z', [ [ (-1, 1), (1, 1), (-1, -1), (1, -1) ] ] )
  , ( '0', [ [ (-1, -0.5), (-0.5, -1), ( 0.5, -1), (1, -0.5)
             , ( 1,  0.5), ( 0.5,  1), (-0.5,  1), (-1, 0.5)
             , (-1, -0.5) ]
           , [ (0, 0.5), (0, -0.5) ] ] )
  , ( '1', [ [ (-0.5, 0.5), (0, 1), (0, -1)] ] )
  , ( '2', [ [ (-1, 0.5), (-0.5, 1), (0.5, 1), (1, 0.5)
             , (-1, -1), (1, -1) ] ] )
  , ( '3', [ [ (-1, 0.75), (-0.5, 1), (0.5, 1), (1, 0.75)
             , (1, 0.5), (-0.25, 0.25), (0.5, 0), (1, -0.25)
             , (1, -0.5), (0.5, -1), (-0.5, -1), (-1, -0.5) ] ] )
  , ( '4', [ [ (1, -0.5), (-1, -0.5), (0.5,1), (0.5,-1) ] ] )
  , ( '5', [ [ (1, 1), (-1, 1), (-1, 0), (-0.5, 0.25), (0.5, 0.25)
             , (1, 0), (1, -0.5), (0.5, -1), (-0.5, -1), (-1, -0.5) ] ] )
  , ( '6', [ [ (-1, -0.25), (-0.5, 0.25), (0.5, 0.25), (1, -0.25)
             , (1, -0.5), (0.5, -1), (-0.5, -1), (-1, -0.5), (-1, 0.5)
             , (0, 1), (1, 1) ] ] )
  , ( '7', [ [ (-1, 1), (1, 1), (0, -1) ] ] )
  , ( '8', [ [ (-1, 0.5), (-0.5, 1), (0.5, 1), (1, 0.5),
               (-1, -0.5), (-0.5, -1), (0.5, -1), (1, -0.5),
               (-1, 0.5) ] ] )
  , ( '9', [ [ (1, 0.25), (0.5, 0), (-0.5, 0), (-1, 0.25), (-1, 0.5)
             , (-0.5, 1), (0.5, 1), (1, 0.5), (1, 0), (0.5, -0.75)
             , (-1, -1) ] ] )
  , ( '@', [ [ (1, -1), (-1, -1), (-1, 1), (1, 1), (1, -0.5)
             , (0.5, -0.5), (0.5, 0.5), (-0.5, 0.5), (-0.5, -0.5)
             , (0.5, -0.25) ] ] )
  , ( '*', [ [ (-0.75,0), (0.75,0) ]
           , [ (-0.5,-0.5), (0.5,0.5) ]
           , [ (0.5,-0.5), (-0.5,0.5) ] ] )
  , ( ' ', [] )
  ]

module Asteroids.UILogic.TextScreen
  ( TextScreen, newScreen, textCursor, textScreenSize, textScreenLetters
  , TextPos, textRow, textCell
  , TextCell, cellColor, cellChar
  , clearScreen, homeScreen, scrollUp, cursorTo
  , writeString, writeChar
  , pokeTextCell, peekTextCell

  , module Asteroids.UILogic.Letters
  ) where

import           Asteroids.UILogic.Drawable
import           Asteroids.UILogic.Letters
import qualified Data.HashMap.Lazy          as H
import           Data.List
import           Data.Maybe                 (fromMaybe)

type TextPos = (Int,Int)
textRow, textCell :: TextPos -> Int
textRow (r,_) = r
textCell (_,c) = c

type TextCell = (PixelColor,Char)
cellColor :: TextCell -> PixelColor
cellChar :: TextCell -> Char
cellColor (c, _) = c
cellChar (_,ch) = ch

type TextContent = H.HashMap TextPos (PixelColor,Char)

data TextScreen = TextScreen
  { textCursor    :: (Int,Int)
  , cursorColor   :: PixelColor
  , textContent   :: TextContent
  , textScreenSize    :: TextPos
  , textScreenLetters :: DrawableLetters }

instance Drawable TextScreen where
  draw t = mapM_ (drawTextCell t) $ textCellToPt2 t <$> H.toList (textContent t)

textCellToPt2 :: TextScreen -> (TextPos, TextCell) -> (Pt2 Coord, TextCell)
textCellToPt2 t (pos, c) = (letterGridPt2 (textScreenLetters t) pos, c)

drawTextCell :: TextScreen -> (Pt2 Coord, TextCell) -> IO ()
drawTextCell t (pos, (color, ch))
  | isDrawableLetter ch (textScreenLetters t) = innerDrawing $ do
      moveTo pos
      draw color
      drawLetter (textScreenLetters t) ch
      return ()
  | otherwise = return ()

newScreen :: TextPos -> LetterScale -> TextScreen
newScreen sizeS sizeC = TextScreen
  { textCursor = (0,0)
  , cursorColor = whitePixel
  , textContent = H.empty
  , textScreenSize = sizeS
  , textScreenLetters = toDrawableLetters letterShapes sizeS sizeC }

adjustContent :: (TextPos -> TextCell -> Maybe (TextPos, TextCell))
                -> TextContent -> TextContent
adjustContent f t = let
  adjustF (Just (pos',ch')) t' = H.insert pos' ch' t'
  adjustF Nothing t'           = t'
  adjustment t' k a = adjustF (f k a) t'
  in H.foldlWithKey' adjustment H.empty t

clearScreen :: TextScreen -> TextScreen
clearScreen t = t { textContent = H.empty }

homeScreen :: TextScreen -> TextScreen
homeScreen = cursorTo 0 0

cursorTo :: Int -> Int -> TextScreen -> TextScreen
cursorTo r c t = t { textCursor = (r,c) }

scrollUp :: Int -> TextScreen -> TextScreen
scrollUp n t = t { textContent = adjustContent upward (textContent t) }
  where upward (r,c) ch | r < n = Nothing
                        | otherwise = Just ((r - n, c), ch)

writeString :: String -> TextScreen -> TextScreen
writeString s t = foldl' (flip writeChar) t s

writeChar :: Char -> TextScreen -> TextScreen
writeChar c t | isDrawableLetter c (textScreenLetters t) = let
                  t' = pokeTextCell (textCursor t) (cursorColor t, c) t
                  in cursorRight t'
              | c == '\n' = lineFeed t
              | otherwise = t

pokeTextCell :: TextPos -> TextCell -> TextScreen -> TextScreen
pokeTextCell pos ch t = t { textContent = H.insert pos ch (textContent t) }

peekTextCell :: TextPos -> TextScreen -> (PixelColor, Char)
peekTextCell pos t = fromMaybe (cursorColor t, ' ') $ H.lookup pos (textContent t)

cursorRight :: TextScreen -> TextScreen
cursorRight t = t'
  where (r, c) = textCursor t
        (_, mc) = textScreenSize t
        t' | c < mc = t { textCursor = (r, c + 1) }
           | otherwise = lineFeed t

lineFeed :: TextScreen -> TextScreen
lineFeed t = t' { textCursor = (r', 0) }
  where (r, _) = textCursor t
        (mr, _) = textScreenSize t
        (t',r') | r < mr = (t, r + 1)
                | otherwise = (scrollUp 1 t, r)

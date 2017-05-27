module Asteroids.UILogic.KeyBindings (
  KeyBindings,
  KeyBinding(..),
  defaultKeyBindings,
  getAction
) where

import           Asteroids.GameLogic.KeyAction
import qualified Data.Hashable                 as H
import           Data.HashMap.Lazy             as HL
import           Graphics.UI.GLUT

newtype KeyBinding = KeyBinding Key
  deriving ( Eq, Show )

instance H.Hashable KeyBinding
  where
    hashWithSalt i _ = i
    hash (KeyBinding k) = hashedK k
      where
        hashedK (Char c) = fromEnum c
        hashedK _        = 0

type KeyBindings = HashMap KeyBinding KeyAction

getAction :: KeyBindings -> Key -> KeyAction
getAction ktab k =
    result found
  where
    found = HL.lookup (KeyBinding k) ktab
    result (Just action) = action
    result Nothing       = Unknown

defaultKeyBindings :: KeyBindings
defaultKeyBindings = fromList [
  (simpleKey $ Char '\ESC',           ExitGame         ),
  (simpleKey $ SpecialKey KeyF11,     ToggleFullScreen ),
  (simpleKey $ SpecialKey KeyLeft,    TurnLeft ),
  (simpleKey $ SpecialKey KeyRight,   TurnRight ),
  (simpleKey $ SpecialKey KeyUp,      Thrust ),
  (simpleKey $ Char ' ',              Fire ),
  (simpleKey $ SpecialKey KeyInsert,  InsertCoin )
  ]

simpleKey :: Key -> KeyBinding
simpleKey = KeyBinding

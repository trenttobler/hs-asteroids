module Asteroids.UILogic.KeyBindings (
  KeyBindings,
  KeyBinding(..),
  defaultKeyBindings,
  getAction
) where

import           Asteroids.GameLogic.KeyAction
import           Data.Hashable
import           Data.HashMap.Lazy
import           Graphics.UI.GLUT

newtype KeyBinding = KeyBinding (Key,Modifiers)
  deriving ( Eq, Show )

instance Hashable KeyBinding
  where
    hashWithSalt i _ = i
    hash (KeyBinding (k,m)) = hashedK k + 1000 * hashedM m
      where
        hashedK (Char c) = fromEnum c
        hashedK _        = 0
        hashedM (Modifiers s c a) = hashedS s + 2 * hashedS c + 4 * hashedS a
        hashedS Down = 1
        hashedS _    = 0

type KeyBindings = HashMap KeyBinding KeyAction

getAction :: KeyBindings -> Key -> Modifiers -> KeyAction
getAction ktab k m =
    result found
  where
    found = Data.HashMap.Lazy.lookup (KeyBinding (k,m)) ktab
    result (Just action) = action
    result Nothing       = Unknown

defaultKeyBindings :: KeyBindings
defaultKeyBindings = fromList [
  (simpleKey $ Char '\ESC',           ExitGame         ),
  (simpleKey $ SpecialKey KeyF11,     ToggleFullScreen ),
  (simpleKey $ SpecialKey KeyLeft,    TurnLeft ),
  (simpleKey $ SpecialKey KeyRight,   TurnRight ),
  (simpleKey $ SpecialKey KeyUp,      Thrust ),
  (simpleKey $ Char ' ',              Fire )
  ]

simpleKey :: Key -> KeyBinding
simpleKey key = KeyBinding (key, Modifiers Up Up Up)

module KeyAction (
  KeyAction(..),
  keyActionName,
  allKeyActions
) where

data KeyAction =
  Unknown
  | ToggleFullScreen
  | ExitGame
  | TurnLeft
  | TurnRight
  | Thrust
  | Fire
  deriving ( Show, Enum )


allKeyActions :: [KeyAction]
allKeyActions = [ Unknown ..]

keyActionName :: KeyAction -> String

keyActionName ToggleFullScreen = "Toggle FullScreen"
keyActionName ExitGame         = "Exit Game"
keyActionName k                = "(Unknown:" ++ show k ++ ")"


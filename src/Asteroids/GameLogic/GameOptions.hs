module Asteroids.GameLogic.GameOptions (
  GameOptions,
  GameOption(..),
  defaultGameOptions,
  hasOption,
  withOption,
  withoutOption,
  toggleOption
) where

data GameOption = FullScreenOption
  deriving (Eq, Show)

newtype GameOptions = GameOptions [GameOption]

defaultGameOptions :: GameOptions
defaultGameOptions = GameOptions []

hasOption :: GameOptions -> GameOption -> Bool
hasOption (GameOptions opts) opt = opt `elem` opts

withOption :: GameOptions -> GameOption -> GameOptions
withOption options opt =
  if hasOption options opt
    then options
    else let (GameOptions without) = options in GameOptions $ opt:without

withoutOption :: GameOptions -> GameOption -> GameOptions
withoutOption options opt =
  if hasOption options opt
    then let (GameOptions with) = options in GameOptions $ filter (/= opt) with
    else options

toggleOption :: GameOptions -> GameOption -> GameOptions
toggleOption options opt =
  if hasOption options opt
    then withoutOption options opt
    else withOption options opt

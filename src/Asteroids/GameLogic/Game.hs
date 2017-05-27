module Asteroids.GameLogic.Game
  ( module Asteroids.GameLogic.Asteroid
  , module Asteroids.GameLogic.Ship
  , Game(..)
  , newGame, gameStep
  , modifyShip, coinInserted
)
where

import           Asteroids.GameLogic.Asteroid
import           Asteroids.GameLogic.Ship
import           Asteroids.UILogic.Drawable
import           Data.List                    (intercalate)
import           Rand

import           Asteroids.UILogic.TextScreen

data PlayStatus
  = SplashScreen
  | PlayingGame
  | GameOver
  deriving (Enum, Eq, Show)

data Game = Game
  { asteroids   :: [Asteroid]
  , gameSeed    :: Int
  , gameLevel   :: Int
  , ship        :: Ship
  , playStatus  :: PlayStatus }

instance Show Game where
  show g = intercalate "\n" ( "--SHIP--"
                            : show (ship g)
                            : ""
                            : "--ASTEROIDS--"
                            : fmap show (asteroids g) )

instance Drawable Game where
  draw game = case playStatus game of
    SplashScreen -> do
      insertCoin
      drawAsteroids'
      
    PlayingGame -> do
      drawShip'
      drawAsteroids'

    GameOver -> do
      gameOver
      insertCoin
      drawShip'
      drawAsteroids'

    where drawAsteroids' = mapM_ draw $ asteroids game
          drawShip' = draw $ ship game

newGame :: Int -> Game
newGame seed = Game
  { asteroids = runSeedRand seed $ createRandomAsteroids firstLevel
  , gameSeed = seed
  , gameLevel = firstLevel
  , ship = createShip (pt2 0 0) 0
  , playStatus = SplashScreen }

firstLevel :: Int
firstLevel = 10

coinInserted :: Game -> Game
coinInserted game =
  case playStatus game of
    PlayingGame -> game
    _ -> game   { asteroids = runSeedRand seed $ createRandomAsteroids level
                , gameSeed = seed
                , gameLevel = level
                , ship = createShip (pt2 0 0) 0
                , playStatus = PlayingGame }
  where seed = gameSeed game + 1
        level = firstLevel

createRandomAsteroids :: Int -> RandomState [Asteroid]
createRandomAsteroids level =
  sequence [ createRandomAsteroid (sz s) s | s <- [1..level] ]
  where sz s = 0.1000 / fromIntegral (1 + (s `mod` 10))

gameStep :: TimeDelta -> Game -> Game
gameStep dt game = case playStatus game of
  SplashScreen -> game { asteroids = stepAsteroids dt game }

  PlayingGame -> game
    { asteroids = stepAsteroids dt game
    , ship = isShipDead $ shipStep dt (ship game)
    , playStatus = if gameOver' then GameOver else PlayingGame }

  GameOver -> game
    { asteroids = stepAsteroids dt game
    , ship = shipStep dt (ship game) }

  where asteroids' = stepAsteroids dt game
        ship' = shipStep dt (ship game)
        isShipDead = if gameOver' then shipIsDead else id
        gameOver' = any (hasCollision ship') asteroids'

  
stepAsteroids :: TimeDelta -> Game -> [Asteroid]
stepAsteroids dt game = fmap (asteroidStep dt) (asteroids game)

hasCollision :: Ship -> Asteroid -> Bool
hasCollision s a = ( asteroidBoundary a `lineBoundaryCrossed` shipBoundary s )
                   && not ( null $ crossedLinePairs (shipWorldLines s)
                                                     (asteroidWorldLines a) )

modifyShip :: (Ship->Ship) -> Game -> Game
modifyShip f game = game { ship = f $ ship game }


textLetters :: DrawableLetters
textLetters = toDrawableLetters letterShapes (25,40) (0.75, 0.75)

bigLetters :: DrawableLetters
bigLetters = toDrawableLetters letterShapes (12,20) (0.75, 0.75)

gameOver :: IO ()
gameOver = do
  draw $ pixelColor 0.5 1 0
  drawCenteredText bigLetters
                   (pt2 0 0)
                   ["Game", "Over"]

insertCoin :: IO ()
insertCoin = do
  draw $ pixelColor 1 1 0
  drawCenteredText textLetters
                   (pt2 0 0.5)
                   ["*** Insert Coin ***"]

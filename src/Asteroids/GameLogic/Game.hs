module Asteroids.GameLogic.Game
  ( module Asteroids.GameLogic.Asteroid
  , module Asteroids.GameLogic.Ship
  , Game(..)
  , newGame, modifyShip, coinInserted
  ) where

import           Asteroids.GameLogic.Asteroid
import           Asteroids.GameLogic.Constants
import           Asteroids.GameLogic.Ship
import           Asteroids.UILogic.Drawable
import           Data.List                    (intercalate, intersperse)
import           Rand

import           Asteroids.UILogic.TextScreen

data PlayStatus
  = SplashScreen
  | PlayingGame
  | GameOver
  deriving (Enum, Eq, Show)

data Game = Game
  { asteroids     :: [Asteroid]
  , gameSeed      :: Int
  , gameLevel     :: Int
  , gameShip      :: Ship
  , playStatus    :: PlayStatus
  , gameLifeCount :: Int
  , gameScore     :: Int }

instance Show Game where
  show g = intercalate "\n" ( show (gameShip g)
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
      drawLifeCount (gameLifeCount game)
      drawScore (gameScore game)

    GameOver -> do
      gameOver
      insertCoin
      drawShip'
      drawAsteroids'
      drawScore (gameScore game)

    where drawAsteroids' = mapM_ draw $ asteroids game
          drawShip' = draw $ gameShip game

instance Physics Game where
  physical g = newPhys pt2Zero pt2Zero 0 0
  step = gameStep
  boundary _ = undefined

newGame :: Int -> Game
newGame seed = Game
  { asteroids = runSeedRand seed $ createRandomAsteroids startingAsteroidCount
  , gameSeed = seed
  , gameLevel = 1
  , gameShip = createShip (pt2 0 0) 0
  , playStatus = SplashScreen
  , gameScore = 0
  , gameLifeCount = 0 }

coinInserted :: Game -> Game
coinInserted game =
  case playStatus game of
    PlayingGame -> game
    _ -> game   { asteroids = runSeedRand seed $ createRandomAsteroids startingAsteroidCount
                , gameSeed = seed
                , gameLevel = level
                , gameShip = createShip (pt2 0 0) 0
                , playStatus = PlayingGame
                , gameScore = 0
                , gameLifeCount = 3 }
  where seed = gameSeed game + 1
        level = 1

createRandomAsteroids :: Int -> RandomState [Asteroid]
createRandomAsteroids level =
  sequence [ createRandomAsteroid (sz s) s | s <- [1..level] ]
  where sz s = 0.1000 / fromIntegral (1 + (s `mod` 10))

gameStep :: TimeDelta -> Game -> Game
gameStep dt game = case playStatus game of
  SplashScreen -> game { asteroids = asteroids' }

  PlayingGame -> game
    { asteroids = asteroids'
    , gameShip = isShipDead ship'
    , playStatus = if gameOver' then GameOver else PlayingGame

    , gameScore = 1 + gameScore game }

  GameOver -> game
    { asteroids = asteroids'
    , gameShip = setBullets bullets' ship' }

  where ship' = step dt (gameShip game)

        asteroids'' = stepAsteroids dt game
        bullets'' = shipBullets ship'
        asteroids' = concatMap (explosions blowUpAsteroid bullets'') asteroids''
        bullets' = bullets'' -- need to get rid of bullets that have collided with asteroids.
        explosions f bs a = if noCollisions bs a then [a] else f a

        gameOver' = any (hasCollision ship') asteroids'
        isShipDead = if gameOver' then shipIsDead else id

stepAsteroids :: TimeDelta -> Game -> [Asteroid]
stepAsteroids dt game = fmap (step dt) (asteroids game)

modifyShip :: (Ship->Ship) -> Game -> Game
modifyShip f game = game { gameShip = f $ gameShip game }

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

drawLifePoly :: IO ()
drawLifePoly = fillPoly $ pt2ToPoly [ Pt2 (-lifeSize,  -lifeSize),
                                      Pt2 ( 0,        2*lifeSize),
                                      Pt2 ( lifeSize,  -lifeSize),
                                      Pt2 ( 0,     -0.9*lifeSize) ]

scoreLetters :: DrawableLetters
scoreLetters = toDrawableLetters letterShapes ( 30, 60 ) (0.75,0.75)

drawScore :: Int -> IO ()
drawScore score = do
  draw scoreColor
  drawRightText scoreLetters scorePos [ score' ]
  where score' = show score

drawLifeCount :: Int -> IO ()
drawLifeCount count = innerDrawing $ do
  draw lifeColor
  moveTo lifePos
  sequence_ (draw' drawLifePoly)
  where draw' = intersperse nextLife' . replicate count
        nextLife' = moveTo lifeSpacing

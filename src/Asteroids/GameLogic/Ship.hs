module Asteroids.GameLogic.Ship
  ( module Asteroids.GameLogic.Physical
  , Ship, ShipRotation(..)
  , createShip
  , shipStep

  , setShipThrust
  , setShipRotation
  , shipWorldLines
  , shipBoundary
  , shipIsDead
  ) where

import           Asteroids.GameLogic.Physical
import           Asteroids.UILogic.Drawable
import           Data.Maybe                   (isNothing)

data ShipRotation = TurnShipLeft
                  | TurnShipRight
                  | StopShipTurning
  deriving (Enum,Show,Eq)

data Ship = Ship
  { shipPos    :: Physical
  , rotateShip :: ShipRotation
  , thrustShip :: Bool
  , shipDiedAt :: Maybe TimeDelta }

instance Show Ship where
  show s = "spin:    " ++ show (rotateShip s) ++ "\n" ++
           "thrust:  " ++ show (thrustShip s) ++ "\n" ++
                          show (shipPos s)

instance Drawable Ship where
  draw ship
    | isNothing $ shipDiedAt ship = innerDrawing $ do
                                    draw (shipPos ship)
                                    drawThrust $ thrustShip ship
                                    shipColor >> drawPoly shipShape
    | otherwise = innerDrawing $ do
        draw (shipPos ship)
        drawExplode $ shipDiedAt ship

createShip :: Position -> Angle -> Ship
createShip pos heading = Ship
  { shipPos = newPhys pos pt2Zero heading 0
              `withSolid` polyPt2Lines shipPoly
  , rotateShip = StopShipTurning
  , thrustShip = False
  , shipDiedAt = Nothing }

shipBoundary :: Ship -> LinePt2 Coord
shipBoundary s = LinePt2 (p0,p1)
  where p = physPos $ shipPos s
        d = maxPt2Dist shipPoly
        p0 = p - pt2 d d
        p1 = p + pt2 d d

shipSize :: Coord
shipSize = 0.02

explosionSize :: Coord
explosionSize = 0.25

burstCount :: Int
burstCount = 7

burstAngle, explosionSpeed :: TimeDelta
burstAngle = 2.0 * pi / fromIntegral burstCount
explosionSpeed = 1

starBurstColor :: PixelColor
starBurstColor = pixelColor 1 0.5 0.2

explodingNear, explodingFar :: TimeDelta -> TimeDelta
explodingNear t = t * t * explosionSize
explodingFar t = ( 2 - t ) * t * explosionSize

starBurst :: TimeDelta -> IO ()
starBurst dt = sequence_ $ fmap burst [0..burstCount]
  where burst n = drawLine (pt2ToPoly (starArm (angle n)))
        starArm a = fmap pt2PolarRadians [(explodingNear dt, a),
                                          (explodingFar dt, a) ]
        angle n = fromIntegral n * burstAngle

drawExplode :: Maybe TimeDelta -> IO ()
drawExplode (Just dt)
  | dt' > 1 = return ()
  | otherwise = do
      draw starBurstColor
      starBurst dt'
  where dt' = dt * explosionSpeed

shipPoly :: [Pt2 Coord]
shipPoly = [ Pt2 (-shipSize, -shipSize),
             Pt2 ( 0,        2*shipSize),
             Pt2 (shipSize,  -shipSize),
             Pt2 ( 0,        -0.9*shipSize) ]

shipShape :: Poly2
shipShape = pt2ToPoly shipPoly

thrustShape :: Poly2
thrustShape = pt2ToPoly [ Pt2 (-shipSize * 0.5, -shipSize * 1.1),
                          Pt2 ( 0,              -shipSize * 1.9),
                          Pt2 ( shipSize * 0.5, -shipSize * 1.1) ]

shipIsDead :: Ship -> Ship
shipIsDead ship
  | isNothing $ shipDiedAt ship = ship { shipDiedAt = Just 0 }
  | otherwise = ship

turnRate :: Coord
turnRate = 360

shipColor :: IO ()
shipColor = drawColor 1 1 1

thrustColor :: IO ()
thrustColor = drawColor 1 0 0

drawThrust :: Bool -> IO ()
drawThrust False = return ()
drawThrust True  = thrustColor >> drawPoly thrustShape

turnShip :: ShipRotation -> Coord ->  Coord
turnShip TurnShipLeft _  = turnRate
turnShip TurnShipRight _ = -turnRate
turnShip _ _             = 0

shipStep :: TimeDelta -> Ship -> Ship
shipStep dt ship
  | isDead $ shipDiedAt ship = shipDied dt ship
  | otherwise = shipUnderControl dt ship
  where isDead Nothing = False
        isDead _       = True

shipUnderControl :: TimeDelta -> Ship -> Ship
shipUnderControl dt ship =  ship { shipPos = p' }
  where p' = step dt forced
        forced = physForce accel torque (shipPos ship)
        torque = turnShip $ rotateShip ship
        accel = if thrustShip ship then goFaster else steady
        steady vel = vel
        unitHeading = getUnitHeading (shipPos ship)
        goFaster vel = vel + mulPt2 unitHeading dt

shipDied :: TimeDelta -> Ship -> Ship
shipDied dt ship =  ship { shipPos = p', shipDiedAt = dt' }
  where p' = step dt forced
        forced = physForce pos' torque (shipPos ship)
        pos' _vel = Pt2 (0,0) -- _vel
        torque = turnShip StopShipTurning
        Just sdt' = shipDiedAt ship
        dt' = Just (dt + sdt')

setShipThrust :: Bool -> Ship -> Ship
setShipThrust b ship = ship { thrustShip = b }

setShipRotation :: ShipRotation -> Ship -> Ship
setShipRotation r ship = ship { rotateShip = r }

shipWorldLines :: Ship -> [LinePt2 Coord]
shipWorldLines ship = solidWorldLines $ shipPos ship

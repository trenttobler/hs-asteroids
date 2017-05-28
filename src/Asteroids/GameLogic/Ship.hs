module Asteroids.GameLogic.Ship
  ( module Asteroids.GameLogic.Physical
  , module Asteroids.GameLogic.Bullet

  , Ship, ShipRotation(..)
  , createShip

  , setShipThrust, setShipRotation, fireBullet, reloadBullet, setBullets

  , shipIsDead, shipBullets
  ) where

import           Asteroids.GameLogic.Physical
import           Asteroids.GameLogic.Bullet
import           Asteroids.GameLogic.Constants
import           Asteroids.UILogic.Drawable
import           Data.Maybe                   (isNothing, catMaybes)

data ShipRotation = TurnShipLeft
                  | TurnShipRight
                  | StopShipTurning
  deriving (Enum,Show,Eq)

data Ship = Ship
  { shipPos    :: Physical
  , rotateShip :: ShipRotation
  , thrustShip :: Bool
  , shipDiedAt :: Maybe TimeDelta
  , readyToFire :: Bool
  , shipBullets :: [Bullet] }

instance Show Ship where
  show s = "--SHIP--\n" ++
           "spin:    " ++ show (rotateShip s) ++ "\n" ++
           "thrust:  " ++ show (thrustShip s) ++ "\n" ++
                          show (shipPos s)

instance Drawable Ship where
  draw ship
    | isNothing $ shipDiedAt ship = innerDrawing $ do
                                      mapM_ draw (shipBullets ship)
                                      draw (shipPos ship)
                                      drawThrust $ thrustShip ship
                                      draw shipColor >> drawPoly shipShape
    | otherwise = innerDrawing $ do
        mapM_ draw (shipBullets ship)
        draw (shipPos ship)
        drawExplode $ shipDiedAt ship

instance Physics Ship where
  physical = shipPos
  step = shipStep
  boundary = worldBoundary (pt2 d d) where d = maxPt2Dist shipPoly

createShip :: Position -> Angle -> Ship
createShip pos heading = Ship
  { shipPos = newPhys pos pt2Zero heading 0
              `withSolid` polyPt2Lines shipPoly
  , rotateShip = StopShipTurning
  , thrustShip = False
  , shipDiedAt = Nothing
  , readyToFire = True
  , shipBullets = [] }

setBullets :: [Bullet] -> Ship -> Ship
setBullets b s = s { shipBullets = b }

fireBullet :: Ship -> Ship
fireBullet ship = if readyToFire ship
                    then ship { shipBullets = bullet' : shipBullets ship
                              , readyToFire = False }
                    else ship
  where bullet' = newBullet (shipPos ship)

reloadBullet :: Ship -> Ship
reloadBullet ship = ship { readyToFire = True }

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
  | otherwise = draw explosionColor >> starBurst dt'
  where dt' = dt * explosionSpeed

shipPoly :: [Pt2 Coord]
shipPoly = [ Pt2 ( -0.5 * shipSize, -0.5 * shipSize),
             Pt2 (    0,                   shipSize),
             Pt2 (  0.5 * shipSize, -0.5 * shipSize),
             Pt2 (    0,           -0.45 * shipSize) ]

shipShape :: Poly2
shipShape = pt2ToPoly shipPoly

thrustShape :: Poly2
thrustShape = pt2ToPoly [ Pt2 (-shipSize * 0.25, -shipSize * 0.55 ),
                          Pt2 ( 0,               -shipSize * 0.85 ),
                          Pt2 ( shipSize * 0.25, -shipSize * 0.55 ) ]

shipIsDead :: Ship -> Ship
shipIsDead ship
  | isNothing $ shipDiedAt ship = ship { shipDiedAt = Just 0 }
  | otherwise = ship

drawThrust :: Bool -> IO ()
drawThrust False = return ()
drawThrust True  = draw thrustColor >> drawPoly thrustShape

turnShip :: ShipRotation -> Coord ->  Coord
turnShip TurnShipLeft _  = shipTurnRate
turnShip TurnShipRight _ = -shipTurnRate
turnShip _ _             = 0

shipStep :: TimeDelta -> Ship -> Ship
shipStep dt ship
  | isDead $ shipDiedAt ship = shipDied dt ship
  | otherwise = shipUnderControl dt ship
  where isDead Nothing = False
        isDead _       = True

shipUnderControl :: TimeDelta -> Ship -> Ship
shipUnderControl dt ship =  ship { shipPos = p'
                                 , shipBullets = stepBullets dt (shipBullets ship) }
  where p' = step dt forced
        forced = physForce accel torque (shipPos ship)
        torque = turnShip $ rotateShip ship
        accel = if thrustShip ship then goFaster else steady
        steady vel = vel
        unitHeading = getUnitHeading (shipPos ship)
        goFaster vel = vel + mulPt2 unitHeading dt

shipDied :: TimeDelta -> Ship -> Ship
shipDied dt ship =  ship { shipPos = p'
                         , shipDiedAt = dt'
                         , shipBullets = stepBullets dt (shipBullets ship) }
  where p' = step dt forced
        forced = physForce pos' torque (shipPos ship)
        pos' _vel = Pt2 (0,0)
        torque = turnShip StopShipTurning
        Just sdt' = shipDiedAt ship
        dt' = Just (dt + sdt')

setShipThrust :: Bool -> Ship -> Ship
setShipThrust b ship = ship { thrustShip = b }

setShipRotation :: ShipRotation -> Ship -> Ship
setShipRotation r ship = ship { rotateShip = r }

stepBullets :: TimeDelta -> [Bullet] -> [Bullet]
stepBullets dt = catMaybes . fmap ( expireMaybe . step dt )
  where expireMaybe b = if bulletAge b > maxBulletAge
                          then Nothing
                          else Just b

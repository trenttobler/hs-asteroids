module Asteroids.GameLogic.Ship
  ( module Asteroids.GameLogic.Physical
  , Ship, ShipRotation(..)
  , createShip
  , shipStep

  , setShipThrust
  , setShipRotation
  ) where

import           Asteroids.GameLogic.Physical
import           Asteroids.UILogic.Drawable

data ShipRotation = TurnShipLeft
                  | TurnShipRight
                  | StopShipTurning
  deriving (Enum,Show,Eq)

data Ship = Ship
  { shipPos       :: Physical
  , rotateShip    :: ShipRotation
  , thrustShip    :: Bool }

instance Show Ship where
  show s = "spin:    " ++ show (rotateShip s) ++ "\n" ++
           "thrust:  " ++ show (thrustShip s) ++ "\n" ++
                          show (shipPos s)

instance Drawable Ship where
  draw ship = innerDrawing $ do
                  draw (shipPos ship)
                  drawThrust $ thrustShip ship
                  shipColor >> drawPoly shipShape

createShip :: Position -> Angle -> Ship
createShip pos heading = Ship
  { shipPos = Physical { physPos = pos
                       , physAngle = heading
                       , physVel = Pt2 (0,0)
                       , physSpin = 0 }
  , rotateShip = StopShipTurning
  , thrustShip = False }

shipSize :: Coord
shipSize = 0.02

shipShape :: Poly2
shipShape = pt2ToPoly [ Pt2 (-shipSize, -shipSize),
                        Pt2 ( 0,        2*shipSize),
                        Pt2 (shipSize,  -shipSize),
                        Pt2 ( 0,        -0.9*shipSize) ]

thrustShape :: Poly2
thrustShape = pt2ToPoly [ Pt2 (-shipSize * 0.5, -shipSize * 1.1),
                          Pt2 ( 0,              -shipSize * 1.9),
                          Pt2 ( shipSize * 0.5, -shipSize * 1.1) ]

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
shipStep dt ship =  ship { shipPos = p' }
  where p' = step dt forced
        forced = physForce accel torque (shipPos ship)
        torque = turnShip $ rotateShip ship
        accel = if thrustShip ship then goFaster else steady
        steady vel = vel
        unitHeading = getUnitHeading (shipPos ship)
        goFaster vel = vel + mulPt2 unitHeading dt

setShipThrust :: Bool -> Ship -> Ship
setShipThrust b ship = ship { thrustShip = b }

setShipRotation :: ShipRotation -> Ship -> Ship
setShipRotation r ship = ship { rotateShip = r }
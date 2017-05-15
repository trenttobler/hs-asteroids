module Asteroids.GameLogic.Ship (
  Ship, ShipState(..), ShipRotation(..),
  createShip, initialShipState,
  shipStep,
) where

import           Asteroids.GameLogic.Physical
import           Asteroids.UILogic.Drawable
import           Pt2

data ShipRotation = TurnShipLeft
                  | TurnShipRight
                  | StopShipTurning
  deriving (Enum,Show,Eq)

data ShipState = ShipState { shipRotation :: ShipRotation, shipThrusting :: Bool }
  deriving (Eq,Show)

data Ship = Ship { ship'Position  :: Physical,
                   ship'Thrusting::Bool }

instance Show Ship where
  show s = "thrust:  " ++ show (ship'Thrusting s) ++ "\n"
                       ++ show (ship'Position s)

instance Drawable Ship where
  draw ship = innerDrawing $ do
                  draw (ship'Position ship)
                  drawThrust (ship'Thrusting ship)
                  shipColor >> drawPoly shipShape

initialShipState :: ShipState
initialShipState = ShipState StopShipTurning False

createShip :: Pt2 Coord -> Coord -> Ship
createShip pos heading = Ship p False
  where p = Physical {
    phys'Position = pos,
    phys'Heading = heading,
    phys'Velocity = Pt2 (0,0),
    phys'Spin = 0
  }

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
drawThrust True  = do
  thrustColor >> drawPoly thrustShape

turnShip :: ShipRotation -> Coord ->  Coord
turnShip TurnShipLeft _  = turnRate
turnShip TurnShipRight _ = (-turnRate)
turnShip _ _             = 0

shipStep :: Ship -> Coord -> ShipState -> Ship
shipStep old dt st =  Ship p' (shipThrusting st)
  where
    p' = physStep dt forced
    forced = physForce (ship'Position old) accel (turnShip $ shipRotation st)
    accel = if shipThrusting st then goFaster else steady
    steady vel = vel
    unitHeading = getUnitHeading (ship'Position old)
    goFaster vel = vel + mulPt2 unitHeading dt

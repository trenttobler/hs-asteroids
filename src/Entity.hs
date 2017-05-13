module Entity (
  makeAsteroid,
  makeShip,
  physStep,
  physForce,
  Entity(..),
  ShipRotation(..),
  ShipState(..),
  initialShipState,
  entityStep
)
where

import           Asteroid
import           Asteroids.GameLogic.Physical
import           Graphics.UI.GLUT
import           Pt2
import           Rand
import           Shapes
import           System.Random
import           Utils

data ShipRotation = TurnShipLeft | TurnShipRight | StopShipTurning
  deriving (Eq,Show,Enum)

data ShipState = ShipState { shipRotation :: ShipRotation, shipThrusting :: Bool }
  deriving (Eq,Show)

initialShipState :: ShipState
initialShipState = ShipState StopShipTurning False

data Entity = PhysicalAsteroid (Asteroid,Physical)
            | PhysicalShip (Physical,Bool)

makeAsteroid :: Double -> Int -> Entity
makeAsteroid size seed = PhysicalAsteroid (createAsteroid size seed r', phys)
  where
    r = seededRandomSeq seed
    (dist,  r1) = randomR (  0.4  ,  1.0    ) r
    (angle, r2) = randomR (  0    ,  2*pi   ) r1
    (dx,    r3) = randomR ( -0.03,  0.03    ) r2
    (dy,    r4) = randomR ( -0.03,  0.03    ) r3
    (spin,  r5) = randomR ( -3    ,  3      ) r4
    r' = r5
    phys = Physical {
      phys'Position = Pt2 (dist * cos angle, dist * sin angle),
      phys'Velocity = Pt2 (dx,dy) `divPt2` sqrt size,
      phys'Heading = 0,
      phys'Spin = spin / size
    }

makeShip :: Pt2 -> Double -> Entity
makeShip pos heading = PhysicalShip (p,False)
  where p = Physical {
    phys'Position = pos,
    phys'Heading = heading,
    phys'Velocity = Pt2 (0,0),
    phys'Spin = 0
  }

instance Show Entity
  where show (PhysicalAsteroid (a,p)) = show a ++ " " ++ show p
        show (PhysicalShip p)         = "Ship at " ++ show p

instance Shape Entity
  where drawGL (PhysicalAsteroid (a,p)) = preservingMatrix $ do
          drawGL p
          drawGL a
        drawGL (PhysicalShip (p,t)) = preservingMatrix $ do
          drawGL p
          renderPrimitive LineLoop shipShape
          drawShipThrust t

drawShipThrust :: Bool -> IO ()
drawShipThrust True = renderPrimitive LineLoop thrustShape
drawShipThrust _    = return ()

-- At some point, will want to break ship out into it's own class.
-- That will also require moving out the physical type to prevent
-- an import loop.
shipSize :: Double
shipSize = 0.02

shipPts :: [Pt2]
shipPts =  [ Pt2 (-shipSize, -shipSize),
             Pt2 ( 0,        2*shipSize),
             Pt2 (shipSize,  -shipSize),
             Pt2 ( 0,        -0.9*shipSize) ]

shipShape :: IO ()
shipShape = mapGLPt2s shipPts

thrustPts :: [Pt2]
thrustPts = [ Pt2 (-shipSize * 0.5, -shipSize * 1.1),
              Pt2 ( 0,              -shipSize * 1.9),
              Pt2 ( shipSize * 0.5, -shipSize * 1.1) ]

thrustShape :: IO ()
thrustShape = mapGLPt2s thrustPts

entityStep :: Double -> ShipState -> Entity -> Entity
entityStep dt _ (PhysicalAsteroid (a,p)) = PhysicalAsteroid (a, physStep dt p )
entityStep dt ship (PhysicalShip (p,_)) = PhysicalShip (p',shipThrusting ship)
  where
    p' = physStep dt forced
    forced = physForce p accel turn
    accel = if shipThrusting ship then goFaster else steady
    steady vel = vel
    unitHeading = getUnitHeading p
    goFaster vel = vel + mulPt2 unitHeading dt
    turn = case shipRotation ship of
      TurnShipLeft  -> const shipRotationRate
      TurnShipRight -> const (-shipRotationRate)
      _             -> const 0

shipRotationRate :: Double
shipRotationRate = 360


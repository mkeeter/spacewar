module Game.Spacewar.Actors where

import Data.Bits (testBit)
import Data.Maybe (mapMaybe)
import System.Random

import Linear.V2
import Linear.Metric
import Linear.Vector

import Game.Spacewar.Physics
import Game.Spacewar.Controls

type Angle = Float
type Thrusting = Bool
type Firing = Bool

data Ship = Ship Object Angle Thrusting Firing |
            DeadShip [Fragment]

-- Helper function used in drawing
rotate :: Float -> [V2f] -> [V2f]
rotate a = let
    ca = cos a
    sa = sin a in
    map (\(V2 x y) -> V2 (ca * x - sa * y) (sa * x + ca * y))

--------------------------------------------------------------------------------

startShip :: Float -> Ship
startShip x = Ship (Object (V2 x 0) (V2 0 0)) 0 False False

shipShape :: [V2f]
shipShape = (map (uncurry V2) $ [(0, 0.1), (-0.05, -0.1), (0.05, -0.1)])
            ++ [head shipShape]

thrustShape :: [V2f]
thrustShape = map (uncurry V2) [(-0.02, -0.1), (0, -0.15), (0.02, -0.1)]

nextShip :: [Shot] -> Control -> Ship -> (Ship, Maybe Shot)
nextShip _ _ (DeadShip f) =
    (DeadShip $ mapMaybe nextFrag f, Nothing)
nextShip shots keys ship@(Ship obj@(Object pos _) t _ fired) =
    if shipHit ship shots then
        let rand = randoms $ mkStdGen (truncate $ 1000 * norm pos)
            start_frag = Fragment (Object (V2 0 0) (V2 0 0)) 0 0 0
            frags = take 10 $ drop 1 $ iterate (\(_, r) -> newFrag ship r)
                                              (start_frag, rand)
        in (DeadShip $ map fst frags, Nothing)
    else
        (Ship (limitSpeed 0.04 $
               sim' obj (if cmd Accel
                         then V2 (-0.001 * sin t) (0.001 * cos t)
                         else V2 0 0))
             (t + (if cmd TurnLeft  then 0.1 else 0)
                - (if cmd TurnRight then 0.1 else 0))
             (cmd Accel) (cmd Shoot),
         if cmd Shoot && not fired
             then Just $ newShot ship
             else Nothing)
    where cmd :: Command -> Bool
          cmd c = testBit keys (fromEnum c)

isPresent :: Ship -> Bool
isPresent (Ship _ _ _ _) = True
isPresent (DeadShip fs) = not $ null fs

shipHit :: Ship -> [Shot] -> Bool
shipHit ship shots = or $ map hit shots
    where hit :: Shot -> Bool
          hit (Shot (Object s _) _ _) = let
            shape = head $ drawShip ship
            edges = zip shape $ drop 1 shape
            vs = [(b - a, s - a) | (a,b) <- edges]
            ds = [(V2 (-y) x) `dot` v | (V2 x y, v) <- vs]
            in and $ map (> 0) ds

drawShip :: Ship -> [[V2f]]
drawShip (Ship (Object pos _) a t _) =
    map (\p -> map (+ pos) $ rotate a p) $
    shipShape:[thrustShape | t]

drawShip (DeadShip fs) = map drawFragment fs

--------------------------------------------------------------------------------

data Shot = Shot Object Float Float

shotShape :: [V2f]
shotShape = [V2 0 (-0.02), V2 0 0.02]

nextShot :: Shot -> Maybe Shot
nextShot (Shot obj a t) = if t <= 0 then Nothing
                                    else Just (Shot (sim obj) a (t - dt))

drawShot :: Shot -> [V2f]
drawShot (Shot (Object pos _) a _) = map (+pos) $ rotate a shotShape

newShot :: Ship -> Shot
newShot (Ship (Object pos _) a _ _) =
    Shot (sim $ Object (pos + (head $ rotate a shipShape))
                       (V2 (-0.07 * sin a) (0.07 * cos a))) a 15
newShot (DeadShip _) = error "Shots cannot be fired from a dead ship"

--------------------------------------------------------------------------------

fragShape :: [V2f]
fragShape = [V2 0 (-0.05), V2 0 0.05]

data Fragment = Fragment Object Angle Float Float
drawFragment :: Fragment -> [V2f]
drawFragment (Fragment (Object pos _) a s t) =
    map (+pos) $ rotate a $ map (^* (s * t/150)) fragShape

newFrag :: Ship -> [Float] -> (Fragment, [Float])
newFrag (Ship (Object pos _) _ _ _) rand =
    let (x:y:dx:dy:a:s:[], rand') = splitAt 6 rand
        pos' = ((V2 x y) - 0.5) ^* 0.1 + pos
        vel' = ((V2 dx dy) - 0.5) ^* 0.01
    in (Fragment (Object pos' vel') (a*2*pi) s 150, rand')
newFrag (DeadShip _) _ = error "Fragments cannot be made from a dead ship"

nextFrag :: Fragment -> Maybe Fragment
nextFrag (Fragment obj a s t) =
    if t <= 0
    then Nothing
    else Just $ Fragment (sim obj) a s (t - dt)


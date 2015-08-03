module Game.Spacewar.Physics where

import Linear.V2
import Linear.Vector
import Linear.Metric

type V2f = V2 Float
data Object = Object V2f V2f

dt :: Float
dt = 1

wrap_ :: Float -> Float
wrap_ x | x < -1 = x + 2
        | x > 1 = x - 2
        | otherwise = x
wrap :: V2f -> V2f
wrap (V2 x y) = V2 (wrap_ x) (wrap_ y)

sim :: Object -> Object
sim (Object pos vel) = Object (wrap $ pos + dt*^vel) vel

sim' :: Object -> V2f -> Object
sim' (Object pos vel) accel = Object (wrap $ pos + dt*^vel) (vel + dt*^accel)

limitSpeed :: Float -> Object -> Object
limitSpeed maxSpeed (Object pos vel) =
    let velNorm = norm vel
        frac = velNorm / maxSpeed
    in Object pos (vel ^/ (max 1 frac))

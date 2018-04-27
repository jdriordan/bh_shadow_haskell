module Type_Defs
( Scalar 
, Vec1
, Vec2
, Vec3
, components
, Integrator(..)
, Coords(..)
) where


import Data.Array.Accelerate hiding (Eq, Scalar, map)
import qualified Data.Array.Accelerate.Numeric.LinearAlgebra as LA

type Scalar = LA.Scalar Double
type Vec1   = LA.Vector Double
type Vec2   = LA.Matrix Double
type Vec3   = Array DIM3 Double

components :: Vec1 -> (Scalar, Scalar, Scalar, Scalar)
components v =
  (x0,x1,x2,x3) where
    (x0:x1:x2:x3:_) = map (\x-> fromList Z [x]) $ toList v
    --                ^ hack to turn these into acc scalars



data Integrator = RK4 | VVerlet deriving (Eq)



-- Schwarzschild, Gullstrand-Painleve, Boyer-Lindquist, Kerr-Schild
data Coords = Schwarzschild | Schwarzschild_GP | Kerr_BL | Kerr_KS deriving (Eq)

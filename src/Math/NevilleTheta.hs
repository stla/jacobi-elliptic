module Math.NevilleTheta
    ( theta_c
    ) where
import Data.Complex
import Math.JacobiTheta

type Cplx = Complex Double

theta_c :: 
     Cplx 
  -> Cplx
  -> Cplx
theta_c z q = 
  jtheta2 z' q / jtheta2 0 q
  where
    j3 = jtheta3 0 q
    z' = z / (j3 * j3)
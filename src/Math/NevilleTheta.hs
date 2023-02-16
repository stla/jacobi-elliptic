module Math.NevilleTheta
    ( theta_c
    ) where
import Data.Complex
import Math.JacobiTheta

type Cplx = Complex Double

-- | Neville theta-c function
theta_c :: 
     Cplx -- ^ z
  -> Cplx -- ^ q, the nome
  -> Cplx
theta_c z q = 
  jtheta2 z' q / jtheta2 0 q
  where
    j3 = jtheta3 0 q
    z' = z / (j3 * j3)
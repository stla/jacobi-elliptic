module Math.NevilleTheta
    ( theta_c, 
      theta_d,
      theta_n,
      theta_s
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

-- | Neville theta-d function
theta_d :: 
     Cplx -- ^ z
  -> Cplx -- ^ q, the nome
  -> Cplx
theta_d z q = 
  jtheta3 z' q / jtheta3 0 q
  where
    j3 = jtheta3 0 q
    z' = z / (j3 * j3)

-- | Neville theta-n function
theta_n :: 
     Cplx -- ^ z
  -> Cplx -- ^ q, the nome
  -> Cplx
theta_n z q = 
  jtheta4 z' q / jtheta4 0 q
  where
    j3 = jtheta3 0 q
    z' = z / (j3 * j3)

-- | Neville theta-d function
theta_s :: 
     Cplx -- ^ z
  -> Cplx -- ^ q, the nome
  -> Cplx
theta_s z q = 
  j3sq * jtheta1 z' q / jtheta1Dash 0 q
  where
    j3 = jtheta3 0 q
    j3sq = j3 * j3
    z' = z / j3sq
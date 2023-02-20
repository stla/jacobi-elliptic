module Math.NevilleTheta
    ( theta_c, 
      theta_d,
      theta_n,
      theta_s,
      theta_c', 
      theta_d',
      theta_n',
      theta_s'
    ) where
import Data.Complex           ( Complex(..) )
import Math.EllipticIntegrals ( ellipticF )
import Math.JacobiTheta
    ( jtheta1, jtheta1Dash, jtheta2, jtheta3, jtheta4 )

type Cplx = Complex Double

i_ :: Cplx
i_ = 0.0 :+ 1.0

tauFromM :: Cplx -> Cplx
tauFromM m = i_ * ellipticF (pi/2) (1 - m) / ellipticF (pi/2) m

nomeFromM :: Cplx -> Cplx
nomeFromM m = exp (i_ * pi * tauFromM m)

-- | Neville theta-c function in terms of the nome.
theta_c :: 
     Cplx -- ^ z
  -> Cplx -- ^ q, the nome
  -> Cplx
theta_c z q = 
  jtheta2 z' q / jtheta2 0 q
  where
    j3 = jtheta3 0 q
    z' = z / (j3 * j3)

-- | Neville theta-d function in terms of the nome.
theta_d :: 
     Cplx -- ^ z
  -> Cplx -- ^ q, the nome
  -> Cplx
theta_d z q = 
  jtheta3 z' q / jtheta3 0 q
  where
    j3 = jtheta3 0 q
    z' = z / (j3 * j3)

-- | Neville theta-n function in terms of the nome.
theta_n :: 
     Cplx -- ^ z
  -> Cplx -- ^ q, the nome
  -> Cplx
theta_n z q = 
  jtheta4 z' q / jtheta4 0 q
  where
    j3 = jtheta3 0 q
    z' = z / (j3 * j3)

-- | Neville theta-d function in terms of the nome.
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

-- | Neville theta-c function in terms of the squared modulus.
theta_c' :: 
     Cplx -- ^ z
  -> Cplx -- ^ m, the squared modulus
  -> Cplx
theta_c' z m = theta_c z (nomeFromM m)

-- | Neville theta-d function in terms of the squared modulus.
theta_d' :: 
     Cplx -- ^ z
  -> Cplx -- ^ m, the squared modulus
  -> Cplx
theta_d' z m = theta_d z (nomeFromM m)

-- | Neville theta-n function in terms of the squared modulus.
theta_n' :: 
     Cplx -- ^ z
  -> Cplx -- ^ m, the squared modulus
  -> Cplx
theta_n' z m = theta_n z (nomeFromM m)

-- | Neville theta-s function in terms of the squared modulus.
theta_s' :: 
     Cplx -- ^ z
  -> Cplx -- ^ m, the squared modulus
  -> Cplx
theta_s' z m = theta_s z (nomeFromM m)

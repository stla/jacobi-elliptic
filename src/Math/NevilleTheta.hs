{-|
Module      : Math.NevilleTheta
Description : Neville theta functions.
Copyright   : (c) Stéphane Laurent, 2023
License     : BSD3
Maintainer  : laurent_step@outlook.fr

Provides the theta Neville functions.
-}
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
    ( jtheta1, jtheta1Dash0, jtheta2, jtheta3, jtheta4 )


i_ :: Complex Double
i_ = 0.0 :+ 1.0

tauFromM :: Complex Double -> Complex Double
tauFromM m = i_ * ellipticF (pi/2) (1 - m) / ellipticF (pi/2) m

nomeFromM :: Complex Double -> Complex Double
nomeFromM m = exp (i_ * pi * tauFromM m)

-- | Neville theta-c function in terms of the nome.
theta_c :: 
     Complex Double -- ^ z
  -> Complex Double -- ^ q, the nome
  -> Complex Double
theta_c z q = 
  jtheta2 z' q / jtheta2 0 q
  where
    j3 = jtheta3 0 q
    z' = z / (j3 * j3)

-- | Neville theta-d function in terms of the nome.
theta_d :: 
     Complex Double -- ^ z
  -> Complex Double -- ^ q, the nome
  -> Complex Double
theta_d z q = 
  jtheta3 z' q / jtheta3 0 q
  where
    j3 = jtheta3 0 q
    z' = z / (j3 * j3)

-- | Neville theta-n function in terms of the nome.
theta_n :: 
     Complex Double -- ^ z
  -> Complex Double -- ^ q, the nome
  -> Complex Double
theta_n z q = 
  jtheta4 z' q / jtheta4 0 q
  where
    j3 = jtheta3 0 q
    z' = z / (j3 * j3)

-- | Neville theta-d function in terms of the nome.
theta_s :: 
     Complex Double -- ^ z
  -> Complex Double -- ^ q, the nome
  -> Complex Double
theta_s z q = 
  j3sq * jtheta1 z' q / jtheta1Dash0 q
  where
    j3 = jtheta3 0 q
    j3sq = j3 * j3
    z' = z / j3sq

-- | Neville theta-c function in terms of the squared modulus.
theta_c' :: 
     Complex Double -- ^ z
  -> Complex Double -- ^ m, the squared modulus
  -> Complex Double
theta_c' z m = theta_c z (nomeFromM m)

-- | Neville theta-d function in terms of the squared modulus.
theta_d' :: 
     Complex Double -- ^ z
  -> Complex Double -- ^ m, the squared modulus
  -> Complex Double
theta_d' z m = theta_d z (nomeFromM m)

-- | Neville theta-n function in terms of the squared modulus.
theta_n' :: 
     Complex Double -- ^ z
  -> Complex Double -- ^ m, the squared modulus
  -> Complex Double
theta_n' z m = theta_n z (nomeFromM m)

-- | Neville theta-s function in terms of the squared modulus.
theta_s' :: 
     Complex Double -- ^ z
  -> Complex Double -- ^ m, the squared modulus
  -> Complex Double
theta_s' z m = theta_s z (nomeFromM m)

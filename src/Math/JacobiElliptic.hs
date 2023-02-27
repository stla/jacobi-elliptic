module Math.JacobiElliptic
    ( jellip,
      jellip',
      am
    ) where
import Data.Complex       ( Complex, realPart, imagPart )
import Math.NevilleTheta
                          ( theta_c,
                            theta_d,
                            theta_n,
                            theta_s,
                            theta_c',
                            theta_d',
                            theta_n',
                            theta_s' )


-- | Jacobi elliptic function in terms of the nome.
jellip :: 
     Char -- ^ a letter among 'c', 'd', 'n', 's' identifying the Neville function at the numerator
  -> Char -- ^ a letter among 'c', 'd', 'n', 's' identifying the Neville function at the denominator
  -> Complex Double -- ^ z, the variable
  -> Complex Double -- ^ q, the nome
  -> Complex Double
jellip p q z nome = 
  theta_num z nome / theta_den z nome
  where
    theta_num = case p of
      'c' -> theta_c
      'd' -> theta_d
      'n' -> theta_n
      's' -> theta_s
      _   -> error "Invalid numerator identifier."
    theta_den = case q of
      'c' -> theta_c
      'd' -> theta_d
      'n' -> theta_n
      's' -> theta_s
      _   -> error "Invalid denominator identifier."

-- | Jacobi elliptic function in terms of the squared modulus.
jellip' :: 
     Char -- ^ a letter among 'c', 'd', 'n', 's' identifying the Neville function at the numerator
  -> Char -- ^ a letter among 'c', 'd', 'n', 's' identifying the Neville function at the denominator
  -> Complex Double -- ^ z, the variable
  -> Complex Double -- ^ m, the squared modulus
  -> Complex Double
jellip' p q z m = 
  theta_num z m / theta_den z m
  where
    theta_num = case p of
      'c' -> theta_c'
      'd' -> theta_d'
      'n' -> theta_n'
      's' -> theta_s'
      _   -> error "Invalid numerator identifier."
    theta_den = case q of
      'c' -> theta_c'
      'd' -> theta_d'
      'n' -> theta_n'
      's' -> theta_s'
      _   -> error "Invalid denominator identifier."

-- | The amplitude function.
am ::
     Complex Double -- ^ u, a complex number 
  -> Complex Double -- ^ m, the squared elliptic modulus
  -> Complex Double
am u m = fromInteger ((-1)^k) * w + k' * pi
  where
    k = round (realPart u / pi) + round (imagPart u / pi)
    k' = fromInteger k
    w = asin (jellip' 's' 'n' u m)

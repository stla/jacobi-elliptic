module Math.JacobiElliptic
    ( jellip,
      jellip'
    ) where
import Data.Complex
import Math.NevilleTheta

type Cplx = Complex Double

-- | Jacobi elliptic function in terms of the nome.
jellip :: 
     Char -- ^ a letter among 'c', 'd', 'n', 's' identifying the Neville function at the numerator
  -> Char -- ^ a letter among 'c', 'd', 'n', 's' identifying the Neville function at the denominator
  -> Cplx -- ^ z, the variable
  -> Cplx -- ^ q, the nome
  -> Cplx
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
  -> Cplx -- ^ z, the variable
  -> Cplx -- ^ m, the squared modulus
  -> Cplx
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
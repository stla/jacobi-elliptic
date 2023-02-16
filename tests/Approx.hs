module Approx where
import           Data.Complex
import           Test.Tasty.HUnit (Assertion, assertEqual)

-- round x to n digits
approx0 :: Int -> Double -> Double
approx0 n x = fromInteger (round $ x * (10^n)) / (10.0^^n)

-- round z to n digits
approx :: Int -> Complex Double -> Complex Double
approx n z = approx0 n (realPart z) :+ approx0 n (imagPart z)

-- check whether rounded z1 and rounded z2 are equal
approxEqual :: Int -> Complex Double -> Complex Double -> Bool
approxEqual n z1 z2 = magnitude (approx n z1 - approx n z2) == 0.0

assertApproxEqual :: String -> Int -> Complex Double -> Complex Double -> Assertion
assertApproxEqual prefix n z1 z2 = 
  assertEqual prefix (approx n z1) (approx n z2)

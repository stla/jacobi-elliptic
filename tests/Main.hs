module Main where
import           Approx
import           Data.Complex
import           Math.NevilleTheta
import           Test.Tasty       (defaultMain, testGroup)
import           Test.Tasty.HUnit (assertBool, testCase)

i_ :: Complex Double
i_ = 0.0 :+ 1.0

z :: Complex Double
z = 1.0 :+ 1.0

q :: Complex Double 
q = exp (-pi)

q' :: Complex Double 
q' = exp (-pi/100)

q'' :: Complex Double 
q'' = exp (i_ * pi * tau)
  where
    tau = 2.0 :+ 2.0

main :: IO ()
main = defaultMain $
  testGroup "Tests"
  [ testCase "theta_c value 1" $ do
      let expected = 0.902705416117337 :+ (-0.718974020880116)
          obtained = theta_c z q
      assertBool "" (approxEqual 10 expected obtained),

    testCase "theta_c value 2" $ do
      let expected = 0.999979735831711 :+ (-0.006366154721717)
          obtained = theta_c z q'
      assertBool "" (approxEqual 10 expected obtained),

    testCase "theta_c value 3" $ do
      let expected = 0.838567437919619 :+ (-0.974584266572289)
          obtained = theta_c z q''
      assertBool "" (approxEqual 10 expected obtained)

  ]

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
q' = exp (-pi/10)

q'' :: Complex Double 
q'' = exp (i_ * pi * tau)
  where
    tau = 2.0 :+ 2.0

main :: IO ()
main = defaultMain $
  testGroup "Tests"
  [ 
    testCase "theta_c value 1" $ do
      let expected = 0.902705416117337 :+ (-0.718974020880116)
          obtained = theta_c z q
      assertApproxEqual "" 10 expected obtained,

    testCase "theta_c value 2" $ do
      let expected = 0.997974260633626 :+ (-0.063618983904188)
          obtained = theta_c z q'
      assertApproxEqual "" 10 expected obtained,

    testCase "theta_c value 3" $ do
      let expected = 0.838567437919619 :+ (-0.974584266572289)
          obtained = theta_c z q''
      assertApproxEqual "" 10 expected obtained,

    testCase "theta_d value 1" $ do
      let expected = 0.892748081976972 :+ (-0.207593861225047)
          obtained = theta_d z q
      assertApproxEqual "" 10 expected obtained,

    testCase "theta_d value 2" $ do
      let expected = 0.997974260633412 :+ (-0.063618983903874)
          obtained = theta_d z q'
      assertApproxEqual "" 10 expected obtained,

    testCase "theta_d value 3" $ do
      let expected = 0.990723180697351 :+ (-0.012164484951676)
          obtained = theta_d z q''
      assertApproxEqual "" 10 expected obtained,

    testCase "theta_n value 1" $ do
      let expected = 1.12730988168993 :+ 0.2469274015421
          obtained = theta_n z q
      assertApproxEqual "" 10 expected obtained,

    testCase "theta_n value 2" $ do
      let expected = 0.894953772623932 :+ 0.933853399701569
          obtained = theta_n z q'
      assertApproxEqual "" 10 expected obtained,

    testCase "theta_n value 3" $ do
      let expected = 1.00934637387594 :+ 0.01225569246714
          obtained = theta_n z q''
      assertApproxEqual "" 10 expected obtained,

    testCase "theta_s value 1" $ do
      let expected = 1.22039326540444 :+ 0.75990704701835
          obtained = theta_s z q
      assertApproxEqual "" 10 expected obtained,

    testCase "theta_s value 2" $ do
      let expected = 0.7162841953585 :+ 1.25543148570321
          obtained = theta_s z q'
      assertApproxEqual "" 10 expected obtained,

    testCase "theta_s value 3" $ do
      let expected = 1.29457805665579 :+ 0.64084576896851
          obtained = theta_s z q''
      assertApproxEqual "" 10 expected obtained,

    testCase "a value of theta_c prime" $ do
      let expected = -0.65900466676738154967
          obtained = theta_c' 2.5 0.3
      assertApproxEqual "" 15 expected obtained,

    testCase "a value of theta_d prime" $ do
      let expected = 0.95182196661267561994
          obtained = theta_d' 2.5 0.3
      assertApproxEqual "" 15 expected obtained,

    testCase "a value of theta_n prime" $ do
      let expected = 1.0526693354651613637
          obtained = theta_n' 2.5 0.3
      assertApproxEqual "" 14 expected obtained,

    testCase "a value of theta_s prime" $ do
      let expected = 0.82086879524530400536
          obtained = theta_s' 2.5 0.3
      assertApproxEqual "" 14 expected obtained

  ]

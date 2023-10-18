module Main where
import           Approx                 ( assertApproxEqual )
import           Data.Complex           ( Complex(..) )
import           Math.NevilleTheta      ( theta_c,
                                          theta_d,
                                          theta_n,
                                          theta_s,
                                          theta_c',
                                          theta_d',
                                          theta_n',
                                          theta_s' )
import           Math.EllipticIntegrals ( ellipticF )
import           Math.JacobiElliptic    ( jellip', am )
import           Test.Tasty             ( defaultMain, testGroup )
import           Test.Tasty.HUnit       ( testCase )

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

u :: Complex Double
u = 0.3 :+ 0.7

m :: Complex Double
m = 0.4 :+ 0.0


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
      let expected = 0.95182196661268
          obtained = theta_d' 2.5 0.3
      assertApproxEqual "" 13 expected obtained,

    testCase "a value of theta_n prime" $ do
      let expected = 1.0526693354651613637
          obtained = theta_n' 2.5 0.3
      assertApproxEqual "" 14 expected obtained,

    testCase "a value of theta_s prime" $ do
      let expected = 0.82086879524530400536
          obtained = theta_s' 2.5 0.3
      assertApproxEqual "" 14 expected obtained,

    testCase "jellip relation 1" $ do
      let z1 = jellip' 'c' 'n' u m 
          z2 = jellip' 'n' 'c' (i_ * u) (1 - m) 
      assertApproxEqual "" 13 z1 z2, 

    testCase "jellip relation 2" $ do
      let z1 = jellip' 's' 'n' u m 
          z2 = -i_ * jellip' 's' 'c' (i_ * u) (1 - m) 
      assertApproxEqual "" 14 z1 z2, 

    testCase "jellip relation 3" $ do
      let z1 = jellip' 'd' 'n' u m 
          z2 = jellip' 'd' 'c' (i_ * u) (1 - m) 
      assertApproxEqual "" 13 z1 z2,

    testCase "amplitude function" $ do
      let phi = 1 :+ 1
          ell = ellipticF phi 0.2
          obtained = am ell 0.2
      assertApproxEqual "" 14 obtained phi

  ]

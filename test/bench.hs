{-# LANGUAGE DataKinds #-}
import Static.Matrix
import Static.Array
import Static.Build
import Criterion.Main
import qualified Linear as L
import Linear (V4(..), M44)
import qualified Numeric.LinearAlgebra as H
import Control.DeepSeq

lm1 :: M44 Double
lm1 = V4 (V4 1 2 3 4)
         (V4 5 6 7 8)
         (V4 9 10 11 12)
         (V4 13 14 15 16)

lm2 :: M44 Double
lm2 = V4 (V4 21 22 23 24)
         (V4 25 26 27 28)
         (V4 29 210 211 212)
         (V4 213 214 215 216)

instance NFData (Array n a)
instance NFData (V4 a)

a :: Matrix 3 2 Double
a = matrix 1 0 (-2)
           0 3 (-1)

b :: Matrix 2 3 Double
b = matrix  0    3
          (-2) (-1)
            0    4

m1 :: Matrix 4 4 Double
m1 = matrix
    1 2 3 4
    5 6 7 8
    9 10 11 12
    13 14 15 16

m2 :: Matrix 4 4 Double
m2 = matrix
    21 22 23 24
    25 26 27 28
    29 210 211 212
    213 214 215 216

hm1 :: H.Matrix Double
hm1 = H.fromLists
    [[1,2,3,4]
    ,[5,6,7,8]
    ,[9,10,11,12]
    ,[13,14,15,16]
    ]

hm2 :: H.Matrix Double
hm2 = H.fromLists
    [[21,22,23,24]
    ,[25,26,27,28]
    ,[29,210,211,212]
    ,[213,214,215,216]
    ]

main :: IO ()
main = defaultMain
  [ bench "linear !*!" (uncurry (L.!*!) `nf` (lm1, lm2))
  , bench "static mXm" (uncurry (·) `nf` (m1, m2))
  , bench "hmatrix mXm" (uncurry H.mXm `nf` (hm1, hm2))
  ]

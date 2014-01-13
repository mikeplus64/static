{-# LANGUAGE DataKinds #-}
module Static.Vector where
import Static.Internal
import Static.Array
import Static.Build
import Foreign

{-# INLINE cross #-}
-- | Cross product
cross :: (Storable a, Num a) => Vector 3 a -> Vector 3 a -> Vector 3 a
cross = \a b ->
  vector
    (a!>1 * b!>2 - a!>2 * b!>1)
    (a!>2 * b!>0 - a!>0 * b!>2)
    (a!>0 * b!>1 - a!>1 * b!>0)

{-# INLINE (×) #-}
-- | Cross product
(×) :: (Storable a, Num a) => Vector 3 a -> Vector 3 a -> Vector 3 a
(×) = cross

infixl 7 ×

{-# INLINE test #-}
test :: Vector 3 Float -> Vector 3 Float -> Vector 3 Float
test a b = 2*a + a×b

dong :: Vector 3 Float
dong = test (vector 1 2 3) (vector 4 5 1000)

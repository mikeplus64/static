{-# LANGUAGE DataKinds, PolyKinds, ConstraintKinds, TypeOperators, FlexibleContexts #-}
module Static.Vector where
import Data.Index hiding (size)
import Static.Array
import Static.Build
import Foreign

type Vector n a = Array (n:.Z) a

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

{-# INLINE dot #-}
dot :: (Num a, Static1 (n:.Z) a) => Vector n a -> Vector n a -> a
dot = sfoldZip (\x y acc -> acc + x * y) 0

{-# INLINE quadrance #-}
-- | Distance squared
quadrance :: (Num a, Static1 (n:.Z) a) => Vector n a -> Vector n a -> a
quadrance = sfoldZip (\x y acc -> acc + x * y) 0

{-# INLINE distanceSq #-}
-- | Vector distance
distanceSq :: (Num a, Static1 (n:.Z) a) => Vector n a -> Vector n a -> a
distanceSq = sfoldZip (\x y acc -> let xy2 = x - y
                                   in xy2 `seq` acc + xy2*xy2) 0

{-# INLINE distance #-}
distance :: (Floating a, Static1 (n:.Z) a) => Vector n a -> Vector n a -> a
distance = \a b -> sqrt (distanceSq a b)

{-# INLINE enorm #-}
-- | Euclidean norm (sum of each component squared)
enorm :: (Floating a, Static1 (n:.Z) a) => Vector n a -> a
enorm = sfoldl' (\x acc -> acc + x*x) 0

{-# INLINE normalise #-}
normalise :: (Fractional a, Storable a) => Vector 4 a -> Vector 3 a
normalise v = w `seq` vector (v!>0/w) (v!>1/w) (v!>2/w)
 where w = v!>3


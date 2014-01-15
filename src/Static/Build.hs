{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, TypeOperators, FunctionalDependencies, ScopedTypeVariables, FlexibleInstances, FlexibleContexts, UndecidableInstances, OverlappingInstances #-}
-- | Efficient initialising functions for static arrays.
module Static.Build
  ( array
  , vector
  , matrix
  , ok
  , Building()
  , Build()
  , Result
  , VectorSize
  , Count
  ) where
import Static.Array
import Static.Internal
import Data.Index.Nat
import qualified Data.Index as Ix
import Data.Index hiding (size)
import GHC.TypeLits
import Data.Proxy
import Data.Tagged
import Foreign
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Control.Monad.Primitive (unsafeInlineIO)
import Unsafe.Coerce

type family Count a :: Peano where
  Count (a -> b)    = Succ (Count b)
  Count (Array n a) = Zero

type family Result a where
  Result (a -> b)    = Result b
  Result (Array n a) = Array n a

-- it could be worse (really)

class Storable a => Build f a | f -> a where
  build_ :: IO (Result f) -> Tagged (Count f) (Ptr a) -> Int -> f

instance ( Count (a -> b) ~ Succ (Count b)
         , Build b a
         ) => Build (a -> b) a where

  {-# INLINE build_ #-}
  build_ acc (Tagged ptr :: Tagged (Succ (Count b)) (Ptr a)) i (x :: a)
    = build_ (pokeElemOff ptr i x >> acc)
             (Tagged ptr :: Tagged (Count b) (Ptr a))
             (i + 1)

instance Storable a => Build (Array n a) a where
  {-# INLINE build_ #-}
  build_ acc _ _ = unsafeInlineIO acc

class (Dim n, Build f a, Result f ~ Array n a, Count f ~ ToPeano (Size n)) => Building n f a
instance (Dim n, Build f a, Result f ~ Array n a, Count f ~ ToPeano (Size n)) => Building n f a

{- | Build an array of arbitrary dimensions. When this is fully inlined or specialised it compiles to a very nice array initialisation.

Examples:

> array 1 2 3 4 :: Vector 4 Double

> array 1 2 3
>       4 5 6
>       7 8 9 :: Matrix 3 3 Double

>>> array 0.1 :: Matrix 4 4 Float
<interactive>:9:1:
    Couldn't match type ‛'Succ (ToPeano (15 - 1))’ with ‛'Zero’
    In the expression: array 0.1 :: Matrix 4 4 Float
    In an equation for ‛it’: it = array 0.1 :: Matrix 4 4 Float

-} 
{-# INLINE array #-}
array :: forall n f a. Building n f a => f
array = build_
    (do touchForeignPtr fptr
        return (Array fptr :: Array n a))
    (Tagged ptr :: Tagged (ToPeano (Size n)) (Ptr a))
    0
  where
    fptr :: ForeignPtr a
    fptr = unsafeInlineIO (mallocForeignPtrArray (Ix.size (Proxy :: Proxy n)))
    ptr  = unsafeForeignPtrToPtr fptr

-- | 'array' specialised to matrices.
{-# INLINE matrix #-}
matrix :: Building (x:.y:.Z) f a => f
matrix = array

type family VectorSize f where
  VectorSize (a -> b)    = 1 + VectorSize b
  VectorSize (Array n a) = 0

{- | 'array' specialised to vectors

>>> vector 1 2 3 4 :: Vector 4 Double
[1,2,3,4]

>>> vector 1 2 3 4 :: Vector 3 Double
<interactive>:8:1:
   Couldn't match type ‛3’ with ‛4’
   In the expression: vector 1 2 3 4 :: Vector 3 Double
   In an equation for ‛it’: it = vector 1 2 3 4 :: Vector 3 Double

-}
{-# INLINE vector #-}
vector :: (x ~ VectorSize f, Building (x:.Z) f a) => f
vector = array

-- | Syntactic nicety for 'vector'
{-# INLINE ok #-}
ok :: Array n a -> Array n a
ok = id

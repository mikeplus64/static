{-# LANGUAGE CPP, UndecidableInstances, MultiParamTypeClasses, ScopedTypeVariables, DataKinds, PolyKinds, TypeOperators, FlexibleInstances, FlexibleContexts, BangPatterns, GeneralizedNewtypeDeriving, TypeFamilies, ConstraintKinds, NoMonomorphismRestriction #-}
module Static.Array
  ( Array(..)
  , Dim
  , (:.)(..)
  , Z(..)
    -- * Getting the size
  , size
  , dimOf
    -- * Indexing
  , (!)
  , (!>)
    -- * Construction
  , constant
  , fromList
  , fromVector
    -- * Deconstruction
  , toVector
  , toList
  , matrixToLists
  , withArray
  , unsafeWithArray
  , unsafeArrayToForeignPtr
    -- * Manipulation
  , Static()
  , asVector
    -- * Mapping
  , scale
  , smap
  , amap
    -- * Folding
  , sfoldl'
  , sfoldr'
  , sfoldr
  , afoldl'
  , afoldr'
  , afoldr
    -- * Zips
  , sfoldZip
  , szipWith
  , azipWith
    -- * Util
  , proxyDim
  , withRange
  , withRangeIndices
  , Static1
  , Static2
  , Static3
  ) where

import qualified Data.Index as Index
import Data.Index hiding (size)
import Data.Index.Nat (CNat(..))
import Static.Internal

import Control.Monad.Primitive (unsafeInlineIO)
import System.IO.Unsafe (unsafeInterleaveIO)
import Foreign hiding (withArray)

import Data.Proxy
import GHC.TypeLits

import qualified Data.Vector.Storable as V

import Control.Applicative
import Control.Monad

instance (Storable a, Eq a, Dim n) => Eq (Array n a) where
  {-# INLINE (==) #-}
  {-# INLINE (/=) #-}
  (==) = all2 (==)
  (/=) = any2 (/=)

instance (Storable a, Ord a, Dim n) => Ord (Array n a) where
  {-# INLINE compare #-}
  compare = afoldl2T (\x y -> foldCmp (compare x y)) EQ

data Continue a = Continue !Bool !a

{-# INLINE foldCmp #-}
foldCmp :: Ordering -> Ordering -> Continue Ordering
foldCmp EQ a = Continue True a
foldCmp a  _ = Continue False a

instance (Storable a, Show a, Dim (n:.Z)) => Show (Array (n:.Z) a) where
  show = show . toList
#if __GLASGOW_HASKELL__ >= 707
instance (Storable a, Show a, CNat x, KnownNat x, Dim (x:.y:.Z)) => Show (Array (x:.y:.Z) a) where
#else
instance (Storable a, Show a, CNat x, SingI x, Dim (x:.y:.Z)) => Show (Array (x:.y:.Z) a) where
#endif
  show = show . matrixToLists

matrixToLists :: (CNat x, Dim (x:.y:.Z), Storable a) => Array (x:.y:.Z) a -> [[a]]
matrixToLists arr = chunksOf (cnat (x' arr)) (toList arr)

chunksOf :: Int -> [a] -> [[a]]
chunksOf i = go 0 []
  where
    go !l !t [] = [reverse t]
    go !l !t (x:xs)
      | l /= i    = go (l+1) (x:t) xs
      | otherwise = reverse t : go 1 [x] xs

{-# INLINE afoldr #-}
-- | Avoid this unless laziness is needed, in which case it might still be better to use 'afoldlT'
afoldr :: (Storable a, Dim n) => (a -> b -> b) -> b -> Array n a -> b
afoldr f z arr = unsafeInlineIO (go 0)
  where
    go !i =
      if i < size arr
      then f <$> at' arr i <*> unsafeInterleaveIO (go (i+1))
      else return z

{-# INLINE afoldlT #-}
-- | Strict left fold with early termination
-- When the condition is true, the fold continues
-- When it is false, the fold ends, but returns still the accumulator given
afoldlT :: (Storable a, Dim n) => (b -> a -> (Bool,b)) -> b -> Array n a -> b
afoldlT f z arr = unsafeInlineIO (go z 0)
  where
    go !acc !i =
      if i < size arr
      then do
        x <- at' arr i
        case f acc x of
          (cond, acc')
            | cond      -> go acc' (i+1)
            | otherwise -> return acc'
      else return acc

{-# INLINE afoldr' #-}
-- | Strict right fold
afoldr' :: (Storable a, Dim n) => (a -> b -> b) -> b -> Array n a -> b
afoldr' f z arr = unsafeInlineIO (go 0)
  where
    go !i =
      if i < size arr
      then f <$> at' arr i <*> go (i+1)
      else return z

-- | Arrays that have statically unrollable operations
class Static (n :: Peano) where
  sfoldl'_   :: (Storable a, Storable b) => Int -> (b -> a -> b) -> b -> Array n a -> IO b
  sfoldr'_   :: (Storable a, Storable b) => Int -> (a -> b -> b) -> b -> Array n a -> IO b
  sfoldr_    :: (Storable a, Storable b) => Int -> (a -> b -> b) -> b -> Array n a -> b
  smap_      :: (Storable a, Storable b) => Proxy o -> Int -> ForeignPtr b -> (a -> b) -> Array n a -> IO (Array o b)
  szipWith_  :: (Storable a, Storable b, Storable c) => Proxy o -> Int -> ForeignPtr c -> (a -> b -> c) -> Array n a -> Array n b -> IO (Array o c)
  sfoldZip_  :: (Storable a, Storable b) => Int -> (a -> b -> c -> c) -> c -> Array n a -> Array n b -> IO c
  constant_  :: Storable a => Proxy n -> Proxy o -> Int -> ForeignPtr a -> a -> IO (Array o a)
instance Static Zero where
  {-# INLINE sfoldl'_ #-}
  {-# INLINE sfoldr'_ #-}
  {-# INLINE sfoldr_ #-}
  {-# INLINE smap_ #-}
  {-# INLINE szipWith_ #-}
  {-# INLINE sfoldZip_ #-}
  {-# INLINE constant_ #-}
  sfoldl'_ _ _ !z _ = return z
  sfoldr'_ _ _ !z _ = return z 
  sfoldr_  _ _  z _ = z
  smap_ _ _ fp _ _  = return (Array fp) 
  szipWith_ _ _ fp _ _ _ = return (Array fp)
  sfoldZip_ _ _ z _ _ = return z
  constant_ _ _ _ fp _ = return (Array fp)
instance Static n => Static (Succ n) where
  {-# INLINE sfoldl'_ #-}
  {-# INLINE sfoldr'_ #-}
  {-# INLINE sfoldr_ #-}
  {-# INLINE smap_ #-}
  {-# INLINE szipWith_ #-}
  {-# INLINE sfoldZip_ #-}
  {-# INLINE constant_ #-}
  sfoldl'_ !i f !z arr = do
    x <- at' arr i
    sfoldl'_ (i+1) f (f z x) (predA arr)

  sfoldr'_ !i f !z arr = f <$> at' arr i <*> sfoldr'_ (i+1) f z (predA arr)

  sfoldr_  !i f  z arr = f (unsafeInlineIO (at' arr i)) (unsafeInlineIO (sfoldr'_ (i+1) f z (predA arr)))

  smap_ o !i fp f arr = do
    x <- at' arr i
    -- pokeElemOff (unsafeForeignPtrToPtr fp) i $! f x
    withForeignPtr fp $ \ptr -> pokeElemOff ptr i $! f x
    smap_ o (i+1) fp f (predA arr)

  szipWith_ o !i fp f xs ys = do
    x <- at' xs i
    y <- at' ys i
    -- pokeElemOff (unsafeForeignPtrToPtr fp) i $! f x y
    withForeignPtr fp $ \ptr -> pokeElemOff ptr i $! f x y
    szipWith_ o (i+1) fp f (predA xs) (predA ys)

  sfoldZip_ !i f !z xs ys = do
    x <- at' xs i
    y <- at' ys i
    sfoldZip_ (i+1) f (f x y z) (predA xs) (predA ys)

  constant_ (_ :: Proxy (Succ n)) o !i fp !a = do
    -- pokeElemOff (unsafeForeignPtrToPtr fp) i a        
    withForeignPtr fp $ \ptr -> pokeElemOff ptr i a
    constant_ (Proxy :: Proxy n) o (i+1) fp a

predA :: Array (Succ n) a -> Array n a
predA (Array a) = Array a

-- | Constraints on static array operations
type Static1 n a     = (Storable a, Dim n, Static (ToPeano (Size n)))
-- | Constraints on static array operations
type Static2 n a b   = (Storable a, Storable b, Dim n, Static (ToPeano (Size n)))
-- | Constraints on static array operations
type Static3 n a b c = (Storable a, Storable b, Storable c, Dim n, Static (ToPeano (Size n)))

toPeanoArray :: Array n a -> Array (ToPeano (Size n)) a
toPeanoArray (Array a) = Array a

toPeanoProxy :: Proxy n -> Proxy (ToPeano (Size n))
toPeanoProxy _ = Proxy

{-# INLINE constant #-}
constant :: Static1 n a => a -> Array n a
constant = constant' Proxy
  where
    {-# INLINE constant' #-}
    constant' :: Static1 n a => Proxy n -> a -> Array n a
    constant' n a = unsafeInlineIO $ do
      new <- mallocForeignPtrArray (Index.size n)
      constant_ (toPeanoProxy n) n 0 new a

{-# INLINE sfoldl' #-}
-- | Static strict left fold over an array
-- When it can be inlined or specialised to a specific size, this is extremely efficient
sfoldl' :: Static2 n a b => (b -> a -> b) -> b -> Array n a -> b
sfoldl' f z = \arr -> unsafeInlineIO $
  sfoldl'_ 0 f z (toPeanoArray arr)

{-# INLINE sfoldr' #-}
-- | Static strict right fold over an array
-- When it can be inlined or specialised to a specific size, this is extremely efficient
sfoldr' :: Static2 n a b => (a -> b -> b) -> b -> Array n a -> b
sfoldr' f z = \arr -> unsafeInlineIO $
  sfoldr'_ 0 f z (toPeanoArray arr)

{-# INLINE sfoldr #-}
-- | Static right fold over an array
-- When it can be inlined or specialised to a specific size, this is extremely efficient
sfoldr :: Static2 n a b => (a -> b -> b) -> b -> Array n a -> b
sfoldr f z = \arr -> sfoldr_ 0 f z (toPeanoArray arr)

{-# INLINE smap #-}
-- | Static map over an array
-- When it can be inlined or specialised to a specific size, this is extremely efficient
smap :: Static2 n a b => (a -> b) -> Array n a -> Array n b
smap f = \arr -> unsafeInlineIO $ do
  new <- mallocForeignPtrArray (size arr)
  smap_ (proxyDim arr) 0 new f (toPeanoArray arr)

{-# INLINE szipWith #-}
-- | Static zip over two arrays
-- When it can be inlined or specialised to a specific size, this is extremely efficient
szipWith :: Static3 n a b c => (a -> b -> c) -> Array n a -> Array n b -> Array n c
szipWith f = \xs ys -> unsafeInlineIO $ do
  new <- mallocForeignPtrArray (size xs)
  szipWith_ (proxyDim xs) 0 new f (toPeanoArray xs) (toPeanoArray ys)

{-# INLINE sfoldZip #-}
sfoldZip :: Static2 n a b => (a -> b -> c -> c) -> c -> Array n a -> Array n b -> c
sfoldZip f z = \xs ys -> unsafeInlineIO (sfoldZip_ 0 f z (toPeanoArray xs) (toPeanoArray ys))

{-# INLINE afoldl' #-}
-- | Strict left fold
-- /O(n)/
afoldl' :: (Storable a, Dim n) => (b -> a -> b) -> b -> Array n a -> b
afoldl' f z arr = let !r = go z 0 in r
  where
    go !acc !i =
      if i < size arr
      then case unsafeInlineIO (at' arr i) of
        !x -> go (f acc x) (i + 1)
      else acc

{-# INLINE azipWith #-}
-- | /O(n)/
azipWith :: (Storable a, Storable b, Storable c, Dim n) => (a -> b -> c) -> Array n a -> Array n b -> Array n c
azipWith f xs ys = unsafeInlineIO $ do
  zs <- mallocForeignPtrArray (size xs)
  withForeignPtr zs $ \ ptr ->
    let go !i = when (i < size xs) $ do
          a <- at' xs i
          b <- at' ys i
          pokeElemOff ptr i $! f a b
          go (i+1)
    in go 0
  return (Array zs)

{-# INLINE afoldl2T #-}
afoldl2T :: (Storable a, Storable b, Dim n) => (a -> b -> c -> Continue c) -> c -> Array n a -> Array n b -> c
afoldl2T f z' xs ys = unsafeInlineIO $ do
  let go !i c
        | i < size xs = do
          a <- at' xs i
          b <- at' ys i
          case f a b c of
            Continue cont x
              | cont      -> go (i+1) x
              | otherwise -> return x
        | otherwise = return c
  go 0 z'

{-# INLINE afoldl2 #-}
afoldl2 :: (Storable a, Storable b, Dim n) => (a -> b -> c -> c) -> c -> Array n a -> Array n b -> c
afoldl2 f z' xs ys = unsafeInlineIO $ do
  let go !i !c
        | i < size xs = do
          a <- at' xs i
          b <- at' ys i
          go (i+1) (f a b c)
        | otherwise = return c
  go 0 z'

{-# INLINE all2 #-}
all2 :: (Storable a, Storable b, Dim n) => (a -> b -> Bool) -> Array n a -> Array n b -> Bool
all2 f xs ys = unsafeInlineIO $ do
  let go !i
        | i < size xs = do
          a <- at' xs i
          b <- at' ys i
          if f a b
            then go (i+1)
            else return False
        | otherwise = return True
  go 0

{-# INLINE any2 #-}
any2 :: (Storable a, Storable b, Dim n) => (a -> b -> Bool) -> Array n a -> Array n b -> Bool
any2 f xs ys = unsafeInlineIO $ do
  let go !i
        | i < size xs = do
          a <- at' xs i
          b <- at' ys i
          if f a b
            then return True
            else go (i+1)
        | otherwise = return False
  go 0

{-# INLINE amap #-}
amap :: (Storable a, Storable b, Dim n) => (a -> b) -> Array n a -> Array n b
amap f xs = unsafeInlineIO $ do
  zs <- mallocForeignPtrArray (size xs)
  withForeignPtr zs $  \ptr ->
    let go !i = when (i < size xs) $ do
          x <- at' xs i
          pokeElemOff ptr i $! f x
          go (i+1)
    in go 0
  return (Array zs)
                
{-# INLINE at' #-}
at' :: Storable a => Array n a -> Int -> IO a
at' (Array a) i = withForeignPtr a (\ptr -> peekElemOff ptr i)

{-# INLINE toVector #-}
toVector :: (Storable a, Dim n) => Array n a -> V.Vector a
toVector arr = V.unsafeFromForeignPtr0 (unsafeArrayToForeignPtr arr) (size arr)

{-# INLINE fromVector #-}
fromVector :: (Storable a, Dim n) => V.Vector a -> Array n a
fromVector = Array . fst . V.unsafeToForeignPtr0

{-# INLINE asVector #-}
-- | In many cases this is a very good function to use, as you are able to leverage vector's stream fusion capabilities
asVector :: (Storable a, Storable b, Dim n) => Array n a -> (V.Vector a -> V.Vector b) -> Array n b
asVector arr f = fromVector (f (toVector arr))

toList :: (Storable a, Dim n) => Array n a -> [a]
toList = afoldr' (:) []

{-# INLINE fromList #-}
fromList :: (Storable a, Dim n) => [a] -> Array n a
fromList = fromVector . V.fromList

{-# INLINE size #-}
size :: Dim n => Array n a -> Int
size = Index.size . proxyDim

{-# INLINE proxyDim #-}
proxyDim :: Array n a -> Proxy n
proxyDim _ = Proxy

{-# INLINE dimOf #-}
dimOf :: Dim n => Array n a -> n
dimOf x = reflect `asProxyTypeOf` proxyDim x

{-# INLINE (!) #-}
(!) :: (Storable a, Dim n) => Array n a -> n -> a
(!) arr ix = unsafeInlineIO $ unsafeWithArray arr $ \ptr -> peekElemOff ptr $! toIndex ix

infixr 8 !

{-# INLINE (!>) #-}
(!>) :: (Storable a, Dim n) => Array n a -> Int -> a
arr!>ix = unsafeInlineIO $ unsafeWithArray arr $ \ptr -> peekElemOff ptr ix

{-# INLINE unsafeArrayToForeignPtr #-}
unsafeArrayToForeignPtr :: Array n a -> ForeignPtr a
unsafeArrayToForeignPtr (Array p) = p

{-# INLINE unsafeWithArray #-}
unsafeWithArray :: Storable a => Array n a -> (Ptr a -> IO b) -> IO b
unsafeWithArray (Array p) = withForeignPtr p

{-# INLINE withArray #-}
withArray :: (Dim n, Storable a) => Array n a -> (Ptr a -> IO b) -> IO b
withArray a f = unsafeWithArray a $ \ ptr -> do
  new <- reallocArray ptr (size a)
  x   <- f new
  free new
  return x

x' :: Array (x:.y:.Z) a -> Proxy x
x' _ = Proxy

y' :: Array (x:.y:.Z) a -> Proxy y
y' _ = Proxy

-- | Element-wise
instance (Num a, Static1 n a) => Num (Array n a) where
  (+)         = szipWith (+)
  (-)         = szipWith (-)
  (*)         = szipWith (*)
  abs         = smap abs
  signum      = smap signum
  fromInteger = constant . fromInteger

-- | Element-wise
instance (Fractional a, Static1 n a) => Fractional (Array n a) where
  (/)          = szipWith (/)
  recip        = smap recip
  fromRational = constant . fromRational

scale :: (Num a, Static1 n a) => a -> Array n a -> Array n a
scale x = smap (x *)

{-# LANGUAGE PolyKinds, DataKinds, TypeOperators, FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds, ScopedTypeVariables, BangPatterns, TypeFamilies #-}
module Static.Matrix
  ( Matrix
  , row, column
  , fromRow, fromColumn
  , takeRow, takeColumn
  , identity
  , (!*!), (·)
  , transpose
  ) where
import Static.Array
import Static.Build
import Static.Vector
import Control.Monad.Primitive
import Control.Applicative
import Foreign
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Data.Index.Nat
import qualified Data.Index as Ix
import Data.Index hiding (size)
import Data.Proxy
import Foreign
import Foreign.Marshal.Array
import GHC.TypeLits

type Matrix x y = Array (x:.y:.Z)

{-# INLINE resultantDim #-}
resultantDim :: Matrix m n a -> Matrix p m a -> Proxy (p:.n:.Z)
resultantDim _ _ = Proxy

{-# INLINE newResultant #-}
newResultant :: (Dim (p:.n:.Z), Storable a) => Matrix m n a -> Matrix p m a -> IO (Matrix p n a)
newResultant a b = Array `fmap` mallocForeignPtrArray (Ix.size (resultantDim a b))

{-# INLINE withArrayRange #-}
withArrayRange :: (Ranged n, Applicative m) => Array n a -> (n -> m ()) -> m ()
withArrayRange _ = swithRange Proxy

row :: Vector n a -> Matrix n 1 a
row (Array a) = Array a

column :: Vector n a -> Matrix 1 n a
column (Array a) = Array a

fromRow :: Matrix n 1 a -> Vector n a
fromRow (Array a) = Array a

fromColumn :: Matrix 1 n a -> Vector n a
fromColumn (Array a) = Array a

-- | Matrix multiply
{-# INLINE (!*!) #-}
(!*!)
  :: (Storable a, Num a, Ranged (p:.n:.Z), Ranged (m:.Z), Dim (m:.n:.Z), Dim (p:.m:.Z))
  => Matrix m n a
  -> Matrix p m a
  -> Matrix p n a
(!*!) = \(a :: Matrix m n a) b -> unsafeInlineIO $ do
  ab@(Array new) <- newResultant a b
  let !ptr = unsafeForeignPtrToPtr new
  withArrayRange ab $ \ ij@(j:.i:.Z) -> 
    pokeElemOff ptr (Ix.toIndex ij) $!
      sfoldlRange Proxy (\acc (k:._ :: m:.Z) ->
        acc + a!k:.i:.Z * b!j:.k:.Z) 0
  return ab

{-# INLINE (·) #-}
(·)
  :: (Storable a, Num a, Ranged (p:.n:.Z), Ranged (m:.Z), Dim (m:.n:.Z), Dim (p:.m:.Z))
  => Matrix m n a
  -> Matrix p m a
  -> Matrix p n a
(·) = (!*!)

infixl 8 !*!
infixl 8 ·
  
-- | Tranposition of a matrix
{-# INLINE transpose #-}
transpose :: (Ranged (x:.y:.Z), Dim (y:.x:.Z), Storable a) => Matrix x y a -> Matrix y x a
transpose a = unsafeInlineIO $ do
  new <- mallocForeignPtrArray (size a)
  let !ptr = unsafeForeignPtrToPtr new
  withArrayRange a $ \ix -> do
    x <- peekElemOff ptr (Ix.toIndex (transposeIx ix))
    pokeElemOff ptr (Ix.toIndex ix) x
  return (Array new)

-- | Identity matrix
{-# INLINE identity #-}
identity :: forall n a. (Ranged (n:.n:.Z), CNat n, Storable a, Num a) => Matrix n n a
identity = unsafeInlineIO $ do
  new <- mallocForeignPtrArray (cnat (Proxy :: Proxy n) ^ (2 :: Int))
  let !ptr = unsafeForeignPtrToPtr new
      !mat = Array new :: Matrix n n a
  withArrayRange mat $ \ ix@(x:.y:.Z) -> 
    if x == y
    then pokeElemOff ptr (Ix.toIndex ix) 1
    else pokeElemOff ptr (Ix.toIndex ix) 0
  return mat

{-# INLINE takeRow #-} 
takeRow
  :: (CNat (r*x), CNat y, Ranged (x:.Z), Storable a, r <= (y - 1))
  => Matrix x y a -> Proxy r -> Vector x a
takeRow mat@(Array a :: Matrix x y a) (r :: Proxy r) = unsafeInlineIO $ do
  new <- mallocForeignPtrArray (cnat (Proxy :: Proxy y))
  let !nptr = unsafeForeignPtrToPtr new
      !optr = unsafeForeignPtrToPtr a
      !vec = Array new :: Vector x a
  withArrayRange vec $ \ (y:.Z) -> 
    pokeElemOff nptr y =<< peekElemOff optr (cnat (Proxy :: Proxy (r*x)) + y)
  return vec

{-# INLINE takeColumn #-}
takeColumn
  :: (CNat x, CNat y, CNat c, Ranged (y:.Z), Storable a, c <= (x - 1))
  => Matrix x y a -> Proxy c -> Vector y a
takeColumn mat@(Array a :: Matrix x y a) (c :: Proxy c) = unsafeInlineIO $ do
  new <- mallocForeignPtrArray (cnat (Proxy :: Proxy x))
  let !nptr = unsafeForeignPtrToPtr new
      !optr = unsafeForeignPtrToPtr a
      !vec = Array new :: Vector y a
  withArrayRange vec $ \ (x:.Z) ->
    pokeElemOff nptr x =<< peekElemOff optr (cnat (Proxy :: Proxy c) + x * cnat (Proxy :: Proxy x))
  return vec

transposeIx :: x:.y:.Z -> y:.x:.Z
transposeIx (x:.y:.Z) = y:.x:.Z

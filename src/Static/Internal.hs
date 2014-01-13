{-# LANGUAGE PolyKinds #-}
-- | Apologies to haddock readers.
-- This module is to ensure type safety.
module Static.Internal (Array(..)) where
import Foreign

newtype Array n a = Array (ForeignPtr a)

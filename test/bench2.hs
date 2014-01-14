module Bench2 where
import Static
import qualified Data.Vector as V
import Control.Monad

type VMatrix a = V.Vector (V.Vector a)

mult :: VMatrix Double -> VMatrix Double -> V.Vector Double
mult f g = V.map (\f' -> V.sum (V.zipWith (*) f' (V.concatMap id g))) f

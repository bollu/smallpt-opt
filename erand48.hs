{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.STRef.Unboxed
import Data.Word
import GHC.Float hiding (clamp)
import Text.Printf
import System.IO (withFile, IOMode(..))
data ET a = ET !Word64 !a deriving Functor

newtype Erand48 a = Erand48 { runErand48' :: Word64 -> ET a } deriving Functor

instance Applicative Erand48 where
  pure a = Erand48 \ w -> ET w a
  {-# INLINE pure #-}
  Erand48 mf <*> Erand48 mn = Erand48 \w ->
    let ET w' f = mf w
        ET w'' n = mn w'
    in ET w'' (f n)
  {-# INLINE (<*>) #-}

instance Monad Erand48 where
  m >>= k = Erand48 \ w ->
    let ET w' a = runErand48' m w
    in runErand48' (k a) w'
  {-# INLINE (>>=) #-}



erand48 :: Erand48 Double
erand48 =  Erand48 \ !r ->
  let x' = 0x5deece66d * r + 0xb
      d_word = 0x3ff0000000000000 .|. ((x' .&. 0xffffffffffff) `unsafeShiftL` 4)
      d = castWord64ToDouble d_word - 1.0
  in ET x' d


genErand48 :: Int -> Erand48 [Double]
genErand48 0 = pure []
genErand48 i = do
  d <- erand48
  ds <- genErand48 (i-1)
  return (d:ds)

runWithErand48 :: Int -> Erand48 a -> a
runWithErand48 !y act = 
  let yw = fromIntegral y; prod = yw * yw * yw; 
      ET _ !r = runErand48' act (prod `unsafeShiftL` 32) in r


main :: IO ()
main = do
  let seed = 42
  -- let xs = runWithErand48 seed (genErand48 200)
  let xs = runWithErand48 seed (genErand48 100)
  forM_ (zip [1..] xs) (\((i :: Int), x) -> printf "%d: %.5f\n" i x) --putStrLn $ show i ++ ": " ++ show x)





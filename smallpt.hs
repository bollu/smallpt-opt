{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
module Main (main) where
import Data.Bits
import Data.ByteString.Builder as BB
import Data.Foldable
import Data.Traversable
import Data.Word
import GHC.Float (castWord64ToDouble)
import System.IO (withFile, IOMode(..))
-- position, also color (r,g,b)
data Vec = Vec {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double

instance Num Vec where
  Vec a b c + Vec x y z = Vec (a+x) (b+y) (c+z)
  Vec a b c - Vec x y z = Vec (a-x) (b-y) (c-z)
  Vec a b c * Vec x y z = Vec (a*x) (b*y) (c*z)
  fromInteger v = let v' = fromInteger v in Vec v' v' v'
  abs (Vec a b c) = Vec a b c
  signum (Vec a b c) = Vec (signum a) (signum b) (signum c)

instance Fractional Vec where
  recip (Vec a b c) = Vec (recip a) (recip b) (recip c)
  fromRational v = let v' = fromRational v in Vec v' v' v'

cross :: Vec -> Vec -> Vec
cross (Vec a b c) (Vec x y z) = Vec (b*z-c*y) (c*x-a*z) (a*y-b*x)
(.*) :: Vec -> Double -> Vec
(Vec a b c) .* x = Vec (a*x) (b*x) (c*x)
infixl 7 .*
len :: Vec -> Double
len (Vec a b c) = sqrt $ a*a+b*b+c*c
norm :: Vec -> Vec
norm v = v .* recip (len v)
dot :: Vec -> Vec -> Double
dot (Vec a b c) (Vec x y z) = a*x+b*y+c*z
maxv :: Vec -> Double
maxv (Vec a b c) = max a (max b c)

data Ray = Ray !Vec !Vec -- origin, direction

newtype Refl = Refl Int  -- material types, used in radiance
pattern DIFF,SPEC,REFR :: Refl
pattern DIFF = Refl 0
pattern SPEC = Refl 1
pattern REFR = Refl 2
{-# COMPLETE DIFF, SPEC, REFR #-}

-- radius, position, emission, color, reflection
data Sphere = Sphere {-# UNPACK #-} !Double {-# UNPACK #-} !Vec {-# UNPACK #-} !Vec {-# UNPACK #-} !Vec {-# UNPACK #-} !Refl

intersect :: Ray -> Sphere -> Double
intersect (Ray o d) (Sphere r p _e _c _refl) =
  if det<0
  then 1e20
  else
    let !eps = 1e-4
        !sdet = sqrt det
        !a = b-sdet
        !s = b+sdet
    in if a>eps then a else if s>eps then s else 1e20
  where
    !det = b*b - dot op op + r*r
    !b = dot op d
    !op = p - o

sphLeft, sphRight, sphBack, sphFrnt, sphBotm, sphTop, sphMirr, sphGlas, sphLite :: Sphere
sphLeft  = Sphere 1e5  (Vec (1e5+1) 40.8 81.6)  0 (Vec 0.75 0.25 0.25) DIFF --Left
sphRight = Sphere 1e5  (Vec (99-1e5) 40.8 81.6) 0 (Vec 0.25 0.25 0.75) DIFF --Rght
sphBack  = Sphere 1e5  (Vec 50 40.8 1e5)        0 0.75  DIFF --Back
sphFrnt  = Sphere 1e5  (Vec 50 40.8 (170-1e5))  0 0     DIFF --Frnt
sphBotm  = Sphere 1e5  (Vec 50 1e5 81.6)        0 0.75  DIFF --Botm
sphTop   = Sphere 1e5  (Vec 50 (81.6-1e5) 81.6) 0 0.75  DIFF --Top
sphMirr  = Sphere 16.5 (Vec 27 16.5 47)         0 0.999 SPEC --Mirr
sphGlas  = Sphere 16.5 (Vec 73 16.5 78)         0 0.999 REFR --Glas
sphLite  = Sphere 600  (Vec 50 681.33 81.6)    12 0     DIFF --Lite

clamp :: (Ord p, Num p) => p -> p
clamp x = if x<0 then 0 else if x>1 then 1 else x

toInt :: Double -> BB.Builder
toInt x = BB.intDec (floor (clamp x ** recip 2.2 * 255 + 0.5)) <> BB.char8 ' '

data T = T !Double !Sphere

intersects :: Ray -> T
intersects ray =
    f (f (f (f (f (f (f (f (T (intersect ray sphLeft) sphLeft) sphRight) sphBack) sphFrnt) sphBotm) sphTop) sphMirr) sphGlas) sphLite
  where
    f !(T k' sp) !s' = let !x = intersect ray s' in if x < k' then T x s' else T k' sp

radiance :: Ray -> Int -> Erand48 Vec
radiance ray@(Ray o d) depth = case intersects ray of
  (T 1e20 _) -> return 0
  (T t (Sphere _r p e c refl)) -> do
    let !x = o + d .* t
        !n = norm $ x - p
        !nl = if dot n d < 0 then n else negate n
        !depth' = depth + 1
        continue f = case refl of
          DIFF -> do
            r1 <- (2*pi*) <$> erand48
            r2 <- erand48
            let r2s = sqrt r2
                w@(Vec wx _ _) = nl
                u = norm $ cross (if abs wx > 0.1 then (Vec 0 1 0) else (Vec 1 0 0)) w
                v = w `cross` u
                d' = norm $ u .* (r2s*cos r1) + v .* (sin r1*r2s) + w .* sqrt (1-r2)
            !rad <- radiance (Ray x d') depth'
            return (e + f * rad)

          SPEC -> do
            let d' = d - n .* (2 * dot n d)
            rad <- radiance (Ray x d') depth'
            return (e + f * rad)

          REFR -> do
            let !cos2t = 1-nnt*nnt*(1-ddn*ddn)
                !into = dot n nl > 0                -- Ray from outside going in?
                !nnt = if into then recip 1.5 else 1.5
                !ddn= dot d nl
                reflRay = Ray x (d - n .* dot (2*n) d) -- Ideal dielectric REFRACTION
            if cos2t<0    -- Total internal reflection
              then do
                !rad <- radiance reflRay depth'
                return (e + f * rad)
              else do
                let tdir = norm (d .* nnt - (n.*((if into then 1 else -1)*(ddn*nnt+sqrt cos2t))))
                    !r0=4.0e-2
                    !c' = 1-if into then -ddn else dot tdir n
                    !re=r0+(1-r0)*c'*c'*c'*c'*c'
                    !tr=1-re
                    !pp=0.25+0.5*re
                    !rp=re/pp
                    !tp=tr/(1-pp)
                rad <-
                  if depth'>2
                    then do er <- erand48
                            if er<pp -- Russian roulette
                              then (.* rp) <$> radiance reflRay depth'
                              else (.* tp) <$> radiance (Ray x tdir) depth'
                    else do !rad0 <- (.* re) <$> radiance reflRay depth'
                            !rad1 <- (.* tr) <$> radiance (Ray x tdir) depth'
                            return (rad0 + rad1)
                return (e + f * rad)

    if depth'>5
      then do
        er <- erand48
        let !pr = maxv c
        if er < pr then continue (c .* recip pr) else return e
      else continue c

smallpt :: Int -> Int -> Int -> IO ()
smallpt w h nsamps = do
  let samps = nsamps `div` 4
      org = Vec 50 52 295.6
      dir = norm $ Vec 0 (-0.042612) (-1)
      cx = Vec (fromIntegral w * 0.5135 / fromIntegral h) 0 0
      cy = norm (cx `cross` dir) .* 0.5135
      img = (`concatMap` [(h-1),(h-2)..0]) $ \y -> runWithErand48 y do
        for [0..w-1] \x -> do
          (\pf -> foldlM pf 0 [(sy, sx) | sy <- [0,1], sx <- [0,1]]) \ci (sy, sx) -> do
            Vec rr rg rb <- (\f -> foldlM f 0 [0..samps-1]) \ !r _s -> do
              r1 <- (2*) <$> erand48
              let !dx = if r1<1 then sqrt r1-1 else 1-sqrt(2-r1)
              r2 <- (2*) <$> erand48
              let !dy = if r2<1 then sqrt r2-1 else 1-sqrt(2-r2)
                  !d = (cx .* (((sx + 0.5 + dx)/2 + fromIntegral x)/fromIntegral w - 0.5)) +
                       (cy .* (((sy + 0.5 + dy)/2 + fromIntegral y)/fromIntegral h - 0.5)) + dir
              rad <- radiance (Ray (org+d.*140) (norm d)) 0
              -- Camera rays are pushed forward ^^^^^ to start in interior
              pure (r + rad .* recip (fromIntegral samps))
            pure (ci + Vec (clamp rr) (clamp rg) (clamp rb) .* 0.25)

  withFile "image.ppm" WriteMode $ \hdl -> do
        BB.hPutBuilder hdl $
          BB.string8 "P3\n" <>
          BB.intDec w <> BB.char8 ' ' <> BB.intDec h <> BB.char8 '\n' <>
          BB.intDec 255 <> BB.char8 '\n' <>
          (mconcat $ fmap (\(Vec r g b) -> toInt r <> toInt g <> toInt b) img)

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

runWithErand48 :: Int -> Erand48 a -> a                                                                                                
runWithErand48 !y act = let yw = fromIntegral y; prod = yw * yw * yw; ET _ !r = runErand48' act (prod `unsafeShiftL` 32) in r

erand48 :: Erand48 Double
erand48 =  Erand48 \ !r ->
  let x' = 0x5deece66d * r + 0xb
      d_word = 0x3ff0000000000000 .|. ((x' .&. 0xffffffffffff) `unsafeShiftL` 4)
      d = castWord64ToDouble d_word - 1.0
  in ET x' d

main :: IO ()
main = smallpt 200 200 256

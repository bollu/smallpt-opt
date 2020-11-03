{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}
module Main (main) where
import Data.Foldable
import Data.List (foldl')
import Data.IORef
import qualified Data.Vector.Mutable as VM
import Text.Printf
import Foreign
import Foreign.C.Types
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
maxv (Vec a b c) = maximum [a,b,c]

data Ray = Ray !Vec !Vec -- origin, direction

newtype Refl = Refl Int  -- material types, used in radiance
pattern DIFF,SPEC,REFR :: Refl
pattern DIFF = Refl 0
pattern SPEC = Refl 1
pattern REFR = Refl 2
{-# COMPLETE DIFF, SPEC, REFR #-}

-- radius, position, emission, color, reflection
data Sphere = Sphere {-# UNPACK #-} !Double {-# UNPACK #-} !Vec {-# UNPACK #-} !Vec {-# UNPACK #-} !Vec {-# UNPACK #-} !Refl

intersect :: Ray -> Sphere -> Maybe Double
intersect (Ray o d) (Sphere r p _e _c _refl) =
  if det<0 then Nothing else f (b-sdet) (b+sdet)
  where op = p - o
        eps = 1e-4
        b = dot op d
        det = b*b - dot op op + r*r
        sdet = sqrt det
        f a s = if a>eps then Just a else if s>eps then Just s else Nothing

spheres :: [Sphere]
spheres =
  [ Sphere 1e5  (Vec (1e5+1) 40.8 81.6)  0 (Vec 0.75 0.25 0.25) DIFF --Left
  , Sphere 1e5  (Vec (99-1e5) 40.8 81.6) 0 (Vec 0.25 0.25 0.75) DIFF --Rght
  , Sphere 1e5  (Vec 50 40.8 1e5)        0 0.75  DIFF --Back
  , Sphere 1e5  (Vec 50 40.8 (170-1e5))  0 0     DIFF --Frnt
  , Sphere 1e5  (Vec 50 1e5 81.6)        0 0.75  DIFF --Botm
  , Sphere 1e5  (Vec 50 (81.6-1e5) 81.6) 0 0.75  DIFF --Top
  , Sphere 16.5 (Vec 27 16.5 47)         0 0.999 SPEC --Mirr
  , Sphere 16.5 (Vec 73 16.5 78)         0 0.999 REFR --Glas
  , Sphere 600  (Vec 50 681.33 81.6)    12 0     DIFF]--Lite

clamp :: (Ord p, Num p) => p -> p
clamp x = if x<0 then 0 else if x>1 then 1 else x

toInt :: Double -> Int
toInt x = floor $ clamp x ** recip 2.2 * 255 + 0.5

intersects :: Ray -> (Maybe Double, Sphere)
intersects ray = (k, s)
  where (k,s) = foldl' f (Nothing,undefined) spheres
        f (k',sp) s' = case (k',intersect ray s') of
                  (Nothing,Just x) -> (Just x,s')
                  (Just y,Just x) | x < y -> (Just x,s')
                  _ -> (k',sp)

radiance :: Ray -> Int -> Ptr CUShort -> IO Vec
radiance ray@(Ray o d) depth xi = case intersects ray of
  (Nothing,_) -> return 0
  (Just t,Sphere _r p e c refl) -> do
    let x = o + d .* t
        n = norm $ x - p
        nl = if dot n d < 0 then n else negate n
        pr = maxv c
        depth' = depth + 1
        continue f = case refl of
          DIFF -> do
            r1 <- (2*pi*) <$> erand48 xi
            r2 <- erand48 xi
            let r2s = sqrt r2
                w@(Vec wx _ _) = nl
                u = norm $ cross (if abs wx > 0.1 then (Vec 0 1 0) else (Vec 1 0 0)) w
                v = w `cross` u
                d' = norm $ u .* (r2s*cos r1) + v .* (sin r1*r2s) + w .* sqrt (1-r2)
            rad <- radiance (Ray x d') depth' xi
            return (e + f * rad)

          SPEC -> do
            let d' = d - n .* (2 * dot n d)
            rad <- radiance (Ray x d') depth' xi
            return (e + f * rad)

          REFR -> do
            let reflRay = Ray x (d - n .* dot (2*n) d) -- Ideal dielectric REFRACTION
                into = dot n nl > 0                -- Ray from outside going in?
                nc = 1
                nt = 1.5
                nnt = if into then nc/nt else nt/nc
                ddn= dot d nl
                cos2t = 1-nnt*nnt*(1-ddn*ddn)
            if cos2t<0    -- Total internal reflection
              then do
                rad <- radiance reflRay depth' xi
                return (e + f * rad)
              else do
                let tdir = norm (d .* nnt - (n.*((if into then 1 else -1)*(ddn*nnt+sqrt cos2t))))
                    a=nt-nc
                    b=nt+nc
                    r0=a*a/(b*b)
                    c' = 1-if into then -ddn else dot tdir n
                    re=r0+(1-r0)*c'*c'*c'*c'*c'
                    tr=1-re
                    pp=0.25+0.5*re
                    rp=re/pp
                    tp=tr/(1-pp)
                rad <-
                  if depth>2
                    then do er <- erand48 xi
                            if er<pp -- Russian roulette
                              then (.* rp) <$> radiance reflRay depth' xi
                              else (.* tp) <$> radiance (Ray x tdir) depth' xi
                    else do rad0 <- (.* re) <$> radiance reflRay depth' xi
                            rad1 <- (.* tr) <$> radiance (Ray x tdir) depth' xi
                            return (rad0 + rad1)
                return (e + f * rad)

    if depth'>5
      then do
        er <- erand48 xi
        if er < pr then continue (c .* recip pr) else return e
      else continue c

smallpt :: Int -> Int -> Int -> IO ()
smallpt w h nsamps = do
  let samps = nsamps `div` 4
      org = Vec 50 52 295.6
      dir = norm $ Vec 0 (-0.042612) (-1)
      cx = Vec (fromIntegral w * 0.5135 / fromIntegral h) 0 0
      cy = norm (cx `cross` dir) .* 0.5135
  c <- VM.replicate (w * h) 0
  allocaArray 3 \xi ->
    flip mapM_ [0..h-1] $ \y -> do
      writeXi xi y
      for_ [0..w-1] \x -> do
        let i = (h-y-1) * w + x
        for_ [0..1] \sy -> do
          for_ [0..1] \sx -> do
            r <- newIORef 0
            for_ [0..samps-1] \_s -> do
              r1 <- (2*) <$> erand48 xi
              let dx = if r1<1 then sqrt r1-1 else 1-sqrt(2-r1)
              r2 <- (2*) <$> erand48 xi
              let dy = if r2<1 then sqrt r2-1 else 1-sqrt(2-r2)
                  d = (cx .* (((sx + 0.5 + dx)/2 + fromIntegral x)/fromIntegral w - 0.5)) +
                      (cy .* (((sy + 0.5 + dy)/2 + fromIntegral y)/fromIntegral h - 0.5)) + dir
              rad <- radiance (Ray (org+d.*140) (norm d)) 0 xi
              -- Camera rays are pushed forward ^^^^^ to start in interior
              modifyIORef r (+ rad .* recip (fromIntegral samps))
            ci <- VM.unsafeRead c i
            Vec rr rg rb <- readIORef r
            VM.unsafeWrite c i $ ci + Vec (clamp rr) (clamp rg) (clamp rb) .* 0.25

  withFile "image.ppm" WriteMode $ \hdl -> do
        hPrintf hdl "P3\n%d %d\n%d\n" w h (255::Int)
        flip mapM_ [0..w*h-1] \i -> do
          Vec r g b <- VM.unsafeRead c i
          hPrintf hdl "%d %d %d " (toInt r) (toInt g) (toInt b)

writeXi :: Ptr CUShort -> Int -> IO ()
writeXi xi y = do
  let y' = fromIntegral y
  pokeElemOff xi 0 0
  pokeElemOff xi 1 0
  pokeElemOff xi 2 (y' * y' * y')

foreign import ccall unsafe "erand48"
  erand48 :: Ptr CUShort -> IO Double

main :: IO ()
main = smallpt 200 200 256

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}
module Main (main) where
import qualified Data.Vector.Mutable as VM
import Data.List (foldl')
import Data.IORef
import Text.Printf
import Foreign
import Foreign.C.Types
import System.IO (withFile, IOMode(..))
-- position, also color (r,g,b)
data Vec = Vec {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double

zerov :: Vec
zerov = Vec 0 0 0
addv, subv, mulv, cross :: Vec -> Vec -> Vec
addv (Vec a b c) (Vec x y z) = Vec (a+x) (b+y) (c+z)
subv (Vec a b c) (Vec x y z) = Vec (a-x) (b-y) (c-z)
mulv (Vec a b c) (Vec x y z) = Vec (a*x) (b*y) (c*z)
cross (Vec a b c) (Vec x y z) = Vec (b*z-c*y) (c*x-a*z) (a*y-b*x)
mulvs :: Vec -> Double -> Vec
mulvs (Vec a b c) x = Vec (a*x) (b*x) (c*x)
len :: Vec -> Double
len (Vec a b c) = sqrt $ a*a+b*b+c*c
norm :: Vec -> Vec
norm v = v `mulvs` (1/len v)
dot :: Vec -> Vec -> Double
dot (Vec a b c) (Vec x y z) = a*x+b*y+c*z
maxv :: Vec -> Double
maxv (Vec a b c) = max a (max b c)

data Ray = Ray {-# UNPACK #-} !Vec {-# UNPACK #-} !Vec -- origin, direction

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
  where op = p `subv` o
        eps = 1e-4
        b = op `dot` d
        det = b*b - (op `dot` op) + r*r
        sdet = sqrt det
        f a s = if a>eps then Just a else if s>eps then Just s else Nothing

spheres :: [Sphere]
spheres = let s = Sphere ; z = zerov ; (.*) = mulvs ; v = Vec in
  [ s 1e5 (v (1e5+1) 40.8 81.6)    z (v 0.75 0.25 0.25) DIFF --Left
  , s 1e5 (v (-1e5+99) 40.8 81.6)  z (v 0.25 0.25 0.75) DIFF --Rght
  , s 1e5 (v 50 40.8 1e5)          z (v 0.75 0.75 0.75) DIFF --Back
  , s 1e5 (v 50 40.8 (-1e5+170))   z z                  DIFF --Frnt
  , s 1e5 (v 50 1e5 81.6)          z (v 0.75 0.75 0.75) DIFF --Botm
  , s 1e5 (v 50 (-1e5+81.6) 81.6)  z (v 0.75 0.75 0.75) DIFF --Top
  , s 16.5(v 27 16.5 47)           z ((v 1 1 1).* 0.999) SPEC --Mirr
  , s 16.5(v 73 16.5 78)           z ((v 1 1 1).* 0.999) REFR --Glas
  , s 600 (v 50 (681.6-0.27) 81.6) (v 12 12 12)       z DIFF]--Lite

clamp :: (Ord p, Num p) => p -> p
clamp x = if x<0 then 0 else if x>1 then 1 else x

toInt :: Double -> Int
toInt x = floor $ clamp x ** (1/2.2) * 255 + 0.5

intersects :: Ray -> (Maybe Double, Sphere)
intersects ray = (k, s)
  where (k,s) = foldl' f (Nothing,undefined) spheres
        f (k',sp) s' = case (k',intersect ray s') of
                  (Nothing,Just x) -> (Just x,s')
                  (Just y,Just x) | x < y -> (Just x,s')
                  _ -> (k',sp)

radiance :: Ray -> CInt -> Ptr CUShort -> IO Vec
radiance ray@(Ray o d) depth xi = case intersects ray of
  (Nothing,_) -> return zerov
  (Just t,Sphere _r p e c refl) -> do
    let x = o `addv` (d `mulvs` t)
        n = norm $ x `subv` p
        nl = if n `dot` d < 0 then n else n `mulvs` (-1)
        depth' = depth + 1
        continue f = case refl of
          DIFF -> do
            r1 <- ((2*pi)*) `fmap` erand48 xi
            r2 <- erand48 xi
            let r2s = sqrt r2
                w@(Vec wx _ _) = nl
                u = norm $ (if abs wx > 0.1 then (Vec 0 1 0) else (Vec 1 0 0)) `cross` w
                v = w `cross` u
                d' = norm $ (u`mulvs`(cos r1*r2s)) `addv` (v`mulvs`(sin r1*r2s)) `addv` (w`mulvs`sqrt (1-r2))
            rad <- radiance (Ray x d') depth' xi
            return $ e `addv` (f `mulv` rad)

          SPEC -> do
            let d' = d `subv` (n `mulvs` (2 * (n`dot`d)))
            rad <- radiance (Ray x d') depth' xi
            return $ e `addv` (f `mulv` rad)

          REFR -> do
            let reflRay = Ray x (d `subv` (n `mulvs` (2* n`dot`d))) -- Ideal dielectric REFRACTION
                into = n`dot`nl > 0                -- Ray from outside going in?
                nc = 1
                nt = 1.5
                nnt = if into then nc/nt else nt/nc
                ddn= d`dot`nl
                cos2t = 1-nnt*nnt*(1-ddn*ddn)
            if cos2t<0    -- Total internal reflection
              then do
                rad <- radiance reflRay depth' xi
                return $ e `addv` (f `mulv` rad)
              else do
                let tdir = norm $ (d`mulvs`nnt `subv` (n`mulvs`((if into then 1 else -1)*(ddn*nnt+sqrt cos2t))))
                    a=nt-nc
                    b=nt+nc
                    r0=a*a/(b*b)
                    c' = 1-(if into then -ddn else tdir`dot`n)
                    re=r0+(1-r0)*c'*c'*c'*c'*c'
                    tr=1-re
                    pp=0.25+0.5*re
                    rp=re/pp
                    tp=tr/(1-pp)
                rad <-
                  if depth>2
                    then do er <- erand48 xi
                            if er<pp -- Russian roulette
                              then (`mulvs` rp) `fmap` radiance reflRay depth' xi
                              else (`mulvs` tp) `fmap` radiance (Ray x tdir) depth' xi
                    else do rad0 <- (`mulvs` re) `fmap` radiance reflRay depth' xi
                            rad1 <- (`mulvs` tr) `fmap` radiance(Ray x tdir) depth' xi
                            return $ rad0 `addv` rad1
                return $ e `addv` (f `mulv` rad)

    if depth'>5
      then do
        er <- erand48 xi
        let !pr = maxv c
        if er < pr then continue $ c `mulvs` (1/pr)
                  else return e
      else continue c

smallpt :: Int -> Int -> Int -> IO ()
smallpt w h nsamps = do
  let samps = nsamps `div` 4
      org = Vec 50 52 295.6
      dir = norm $ Vec 0 (-0.042612) (-1)
      cx = Vec (fromIntegral w * 0.5135 / fromIntegral h) 0 0
      cy = norm (cx `cross` dir) `mulvs` 0.5135
  c <- VM.replicate (w * h) zerov
  allocaArray 3 $ \xi ->
      flip mapM_ [0..h-1] $ \y -> do
        --hPrintf stderr "\rRendering (%d spp) %5.2f%%" (samps*4::Int) (100.0*fromIntegral y/(fromIntegral h-1)::Double)
      writeXi xi y
      flip mapM_ [0..w-1] $ \x -> do
        let i = (h-y-1) * w + x
        flip mapM_ [0..1] $ \sy -> do
          flip mapM_ [0..1] $ \sx -> do
            r <- newIORef zerov
            flip mapM_ [0..samps-1] $ \_s -> do
              r1 <- (2*) `fmap` erand48 xi
              let dx = if r1<1 then sqrt r1-1 else 1-sqrt(2-r1)
              r2 <- (2*) `fmap` erand48 xi
              let dy = if r2<1 then sqrt r2-1 else 1-sqrt(2-r2)
                  d = (cx `mulvs` (((sx + 0.5 + dx)/2 + fromIntegral x)/fromIntegral w - 0.5)) `addv`
                      (cy `mulvs` (((sy + 0.5 + dy)/2 + fromIntegral y)/fromIntegral h - 0.5)) `addv` dir
              rad <- radiance (Ray (org`addv`(d`mulvs`140)) (norm d)) 0 xi
              -- Camera rays are pushed forward ^^^^^ to start in interior
              modifyIORef r (`addv` (rad `mulvs` (1 / fromIntegral samps)))
            ci <- VM.unsafeRead c i
            Vec rr rg rb <- readIORef r
            VM.unsafeWrite c i $ ci `addv` (Vec (clamp rr) (clamp rg) (clamp rb) `mulvs` 0.25)

  withFile "image.ppm" WriteMode $ \hdl -> do
        hPrintf hdl "P3\n%d %d\n%d\n" w h (255::Int)
        flip mapM_ [0..w*h-1] $ \i -> do
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

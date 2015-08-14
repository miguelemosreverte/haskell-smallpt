-- Small path tracing with Haskell
import System.Environment
import Control.Monad
import Control.Monad.Random
import Codec.Picture
import Data.Time
import qualified Data.Word as W
import qualified Data.Vector.Storable as V

import Debug.Trace

-- Parameters
eps :: Double
eps = 1.0e-4

inf :: Double
inf = 1.0e20

nc :: Double
nc  = 1.0

nt :: Double
nt  = 1.5

-- Vec
data Vec = Vec (Double, Double, Double) deriving (Show)
instance (Num Vec) where
    (Vec (x, y, z)) + (Vec (a, b, c)) = Vec (x + a, y + b, z + c)
    (Vec (x, y, z)) - (Vec (a, b, c)) = Vec (x - a, y - b, z - c)
    (Vec (x, y, z)) * (Vec (a, b, c)) = Vec (x * a, y * b, z * c)
    abs = undefined
    signum = undefined
    fromInteger x = Vec (dx, dx, dx) where dx = fromIntegral x

x :: Vec -> Double
x (Vec (x, _, _)) = x

y :: Vec -> Double
y (Vec (_, y, _)) = y

z :: Vec -> Double
z (Vec (_, _, z)) = z

mul :: Vec -> Double -> Vec
mul (Vec (x, y, z)) s = Vec (x * s, y * s, z * s)

dot :: Vec -> Vec -> Double
dot (Vec (x, y, z)) (Vec (a, b, c))  = x * a + y * b + z * c

norm :: Vec -> Vec
norm (Vec (x, y, z)) = Vec (x * invnrm, y * invnrm, z * invnrm)
    where invnrm = 1 / sqrt (x * x + y * y + z * z)

cross :: Vec -> Vec -> Vec
cross (Vec (x, y, z)) (Vec (a, b, c)) = Vec (y * c - b * z, z * a - c * x, x * b - a * y)

-- Ray
data Ray = Ray (Vec, Vec) deriving (Show)

org :: Ray -> Vec
org (Ray (org, _)) = org

dir :: Ray -> Vec
dir (Ray (_, dir)) = dir

-- Material
data Refl = Diff
          | Spec
          | Refr
          deriving Show

-- Sphere
data Sphere = Sphere (Double, Vec, Vec, Vec, Refl) deriving (Show)

rad :: Sphere -> Double
rad  (Sphere (rad, _, _, _, _   )) = rad

pos :: Sphere -> Vec
pos  (Sphere (_  , p, _, _, _   )) = p

emit :: Sphere -> Vec
emit (Sphere (_  , _, e, _, _   )) = e

col :: Sphere -> Vec
col  (Sphere (_  , _, _, c, _   )) = c

refl :: Sphere -> Refl
refl (Sphere (_  , _, _, _, refl)) = refl

intersect :: Sphere -> Ray -> Double
intersect sp ray =
    let op  = (pos sp) - (org ray)
        b   = op `dot` (dir ray)
        det = b * b - (op `dot` op) + ((rad sp) ** 2)
    in
        if det < 0.0
            then inf
            else
                let sqdet = sqrt det
                    t1    = b - sqdet
                    t2    = b + sqdet
                in ansCheck t1 t2
                      where ansCheck t1 t2
                                | t1 > eps  = t1
                                | t2 > eps  = t2
                                | otherwise = inf

-- Scene
type Scene = [Sphere]
sph :: Scene
sph = [ Sphere (1e5,  Vec ( 1e5+1,  40.8, 81.6),    Vec (0.0, 0.0, 0.0), Vec (0.75, 0.25, 0.25),  Diff)   -- Left
      , Sphere (1e5,  Vec (-1e5+99, 40.8, 81.6),    Vec (0.0, 0.0, 0.0), Vec (0.25, 0.25, 0.75),  Diff)   -- Right
      , Sphere (1e5,  Vec (50.0, 40.8,  1e5),       Vec (0.0, 0.0, 0.0), Vec (0.75, 0.75, 0.75),  Diff)   -- Back
      , Sphere (1e5,  Vec (50.0, 40.8, -1e5+170),   Vec (0.0, 0.0, 0.0), Vec (0.0, 0.0, 0.0),     Diff)   -- Front
      , Sphere (1e5,  Vec (50, 1e5, 81.6),          Vec (0.0, 0.0, 0.0), Vec (0.75, 0.75, 0.75),  Diff)   -- Bottom
      , Sphere (1e5,  Vec (50,-1e5+81.6,81.6),      Vec (0.0, 0.0, 0.0), Vec (0.75, 0.75, 0.75),  Diff)   -- Top
      , Sphere (16.5, Vec (27, 16.5, 47),           Vec (0.0, 0.0, 0.0), Vec (1,1,1) `mul` 0.999, Spec)   -- Mirror
      , Sphere (16.5, Vec (73, 16.5, 78),           Vec (0.0, 0.0, 0.0), Vec (1,1,1) `mul` 0.999, Refr)   -- Glass
      , Sphere (600,  Vec (50, 681.6 - 0.27, 81.6), Vec (12, 12, 12),    Vec (0, 0, 0),           Diff) ] -- Light

-- Utility functions
clamp :: Double -> Double
clamp = (max 0.0) . (min 1.0)

isectWithScene :: Scene -> Ray -> (Double, Int)
isectWithScene scene ray = foldr1 (min) $ zip [ intersect sph ray | sph <- scene ] [0..]

lambert :: Vec -> Double -> Double -> (Vec, Double)
lambert n r1 r2 =
    let th  = 2.0 * pi * r1
        r2s = sqrt r2
        w = n
        u = norm $ (if (abs (x w)) > eps then Vec (0, 1, 0) else Vec (1, 0, 0)) `cross` w
        v = w `cross` u
        uu = u `mul` ((cos th) * r2s)
        vv = v `mul` ((sin th) * r2s)
        ww = w `mul` (sqrt (1.0 - r2))
        rdir = norm (uu + vv + ww)
    in (rdir, 1)

reflect :: Vec -> Vec -> (Vec, Double)
reflect v n =
    let rdir = v - (n `mul` (2.0 * n `dot` v))
    in (rdir, 1)

refract :: Vec -> Vec -> Vec -> Double -> (Vec, Double)
refract v n orn rr =
    let (rdir, _) = reflect v orn
        into = (n `dot` orn) > 0
        nnt  = if into then (nc / nt) else (nt / nc)
        ddn  = v `dot` orn
        cos2t = 1.0 - nnt * nnt * (1.0 - ddn * ddn)
    in
        if cos2t < 0.0
            then (rdir, 1.0)
            else
                let tdir = norm $ ((v `mul` nnt) -) $ n `mul` ((if into then 1 else -1) * (ddn * nnt + (sqrt cos2t)))
                    a = nt - nc
                    b = nt + nc
                    r0 = (a * a) / (b * b)
                    c = 1.0 - (if into then -ddn else (tdir `dot` n))
                    re = r0 + (1 - r0) * (c ** 5)
                    tr = 1.0 - re
                    pp = 0.25 + 0.5 * re
                in
                    if rr < pp
                         then (rdir, (pp / re))
                         else (tdir, ((1.0 - pp) / tr))

radiance :: (RandomGen g) => Scene -> Ray -> Int -> Rand g Vec
radiance scene ray depth = do
    let (t, i) = (isectWithScene scene ray)
    if inf <= t
        then return (Vec (0, 0, 0))
        else do
            r0 <- getRandomR (0.0, 1.0)
            r1 <- getRandomR (0.0, 1.0)
            r2 <- getRandomR (0.0, 1.0)
            let obj = (scene !! i)
            let c = col obj
            let prob = (max (x c) (max (y c) (z c)))
            if depth >= 5 && r0 >= prob
                then return (emit obj)
                else do
                    let rlt = if depth < 5 then 1 else prob
                    let f = (col obj)
                    let d = (dir ray)
                    let x = (org ray) + (d `mul` t)
                    let n = norm $ x - (pos obj)
                    let orn = if (d `dot` n) < 0.0  then n else (-n)
                    let (ndir, pdf) = case (refl obj) of
                            Diff -> (lambert orn r1 r2)
                            Spec -> (reflect d orn)
                            Refr -> (refract d n orn r1)
                    nextRad <- (radiance scene (Ray (x, ndir)) (succ depth))
                    return $ ((emit obj) + ((f * nextRad) `mul` (1.0 / (rlt * pdf))))

toByte :: Double -> W.Word8
toByte x = truncate (((clamp x) ** (1.0 / 2.2)) * 255.0) :: W.Word8

accumulateRadiance :: (RandomGen g) => Scene -> Ray -> Int -> Rand g Vec
accumulateRadiance scene ray spp = do
    let rays = take spp $ repeat ray
    pixels <- sequence [ (radiance scene r 0) | r <- rays]
    return $ (foldr1 (+) pixels) `mul` (1 / fromIntegral spp)

main :: IO ()
main = do
    args <- getArgs
    let argc = length args
    let w   = if argc >= 1 then (read (args !! 0)) else 400 :: Int
    let h   = if argc >= 2 then (read (args !! 1)) else 300 :: Int
    let spp = if argc >= 3 then (read (args !! 2)) else 4   :: Int

    startTime <- getCurrentTime

    putStrLn "-- Smallpt.hs --"
    putStrLn $ "  width = " ++ (show w)
    putStrLn $ " height = " ++ (show h)
    putStrLn $ "    spp = " ++ (show spp)

    let dw = fromIntegral w :: Double
    let dh = fromIntegral h :: Double

    let cam = Ray (Vec (50, 52, 295.6), (norm $ Vec (0, -0.042612, -1)));
    let cx  = Vec (dw * 0.5135 / dh, 0.0, 0.0)
    let cy  = (norm $ cx `cross` (dir cam)) `mul` 0.5135
    let dirs = [ norm $ (dir cam) + (cy `mul` (y / dh  - 0.5)) + (cx `mul` (x / dw - 0.5)) | y <- [dh-1,dh-2..0], x <- [0..dw-1] ]
    let rays = [ Ray ((org cam) + (d `mul` 140.0), (norm d)) | d <- dirs ]

    gen <- getStdGen
    let pixels = evalRand (sequence [ (accumulateRadiance sph r spp) | r <- rays]) gen :: [Vec]

    let pixelData = map toByte $! pixels `seq` (foldr (\col lst -> [(x col), (y col), (z col)] ++ lst) [] pixels)
    let pixelBytes = V.fromList pixelData :: V.Vector W.Word8
    let img = Image { imageHeight = h, imageWidth = w, imageData = pixelBytes } :: Image PixelRGB8
    writePng "image.png" img

    endTime <- getCurrentTime
    print $ diffUTCTime endTime startTime

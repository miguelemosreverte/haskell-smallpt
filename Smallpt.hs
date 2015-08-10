-- Small path tracing with Haskell
import System.Random
import Control.Monad
import Codec.Picture
import qualified Data.Word as DWord
import qualified Data.Vector.Storable as DVector

-- Parameters
eps = 1.0e-8
inf = 1.0e20
nc  = 1.0
nt  = 1.5

-- Vec
data Vec = Vec (Double, Double, Double)
    deriving (Eq, Show)

x (Vec (x, y, z)) = x
y (Vec (x, y, z)) = y
z (Vec (x, y, z)) = z

Vec (x, y, z) `add` Vec (a, b, c)  = Vec (x + a, y + b, z + c)
Vec (x, y, z) `sub` Vec (a, b, c)  = Vec (x - a, y - b, z - c)
Vec (x, y, z) `mult` Vec (a, b, c) = Vec (x * a, y * b, z * c)
Vec (x, y, z) `mul` s              = Vec (x * s, y * s, z * s)
Vec (x, y, z) `dot` Vec (a, b, c)  = x * a + y * b + z * c
norm (Vec (x, y, z)) = Vec (x / nrm, y / nrm, z / nrm)
    where nrm = sqrt (x * x + y * y + z * z)
Vec (x, y, z) `cross` Vec (a, b, c) = Vec (y * c - b * z, z * a - c * x, x * b - a * y)

-- Ray
data Ray = Ray (Vec, Vec)
    deriving (Show)

org (Ray (o, d)) = o
dir (Ray (o, d)) = d

-- Material
data Refl = Diff
          | Spec
          | Refr
          deriving Show

-- Sphere
data Sphere = Sphere (Double, Vec, Vec, Vec, Refl)
    deriving (Show)

rad  (Sphere (rad, p, e, c, refl)) = rad
pos  (Sphere (rad, p, e, c, refl)) = p
emit (Sphere (rad, p, e, c, refl)) = e
col  (Sphere (rad, p, e, c, refl)) = c
refl (Sphere (rad, p, e, c, refl)) = refl

intersect :: Sphere -> Ray -> Double
intersect sp ray =
    let op  = (pos sp) `sub` (org ray)
        b   = op `dot` (dir ray)
        det = b * b - (op `dot` op) + ((rad sp) ** 2)
    in if det < 0.0 then inf
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
sph :: [Sphere]
sph =  [ Sphere (1e5, Vec ( 1e5+1,  40.8, 81.6), Vec (0.0, 0.0, 0.0), Vec (0.75, 0.25, 0.25), Diff) ] -- Left
    ++ [ Sphere (1e5, Vec (-1e5+99, 40.8, 81.6), Vec (0.0, 0.0, 0.0), Vec (0.25, 0.25, 0.75), Diff) ] -- Right
    ++ [ Sphere (1e5, Vec (50.0, 40.8,  1e5),     Vec (0.0, 0.0, 0.0), Vec (0.75, 0.75, 0.75), Diff) ] -- Back
    ++ [ Sphere (1e5, Vec (50.0, 40.8, -1e5+170), Vec (0.0, 0.0, 0.0), Vec (0.0, 0.0, 0.0),    Diff) ] -- Front
    ++ [ Sphere (1e5, Vec (50, 1e5, 81.6),      Vec (0.0, 0.0, 0.0), Vec (0.75, 0.75, 0.75), Diff) ] -- Bottom
    ++ [ Sphere (1e5, Vec (50,-1e5+81.6,81.6), Vec (0.0, 0.0, 0.0), Vec (0.75, 0.75, 0.75), Diff) ] -- Top
    ++ [ Sphere (16.5, Vec (27, 16.5, 47),        Vec(0.0, 0.0, 0.0), Vec(1,1,1) `mul` 0.999, Spec) ] -- Mirror
    ++ [ Sphere (16.5, Vec (73, 16.5, 78),        Vec(0.0, 0.0, 0.0), Vec(1,1,1) `mul` 0.999, Refr) ] -- Glass
    ++ [ Sphere (600,  Vec (50, 681.6 - 0.27, 81.6), Vec(12,12,12),      Vec(0,0,0), Diff) ] -- Light

-- Utility functions
clamp :: Double -> Double
clamp = (max 0.0) . (min 1.0)
cartProd :: [a] -> [b] -> [(a, b)]
cartProd = liftM2 (,)
enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

isectWithScene :: [Sphere] -> Ray -> (Int, Double)
isectWithScene scene ray =
    let n  = length scene
        ds = map (\sp -> intersect sp ray) scene
    in foldr1 minWithId (enumerate ds)
           where minWithId p q
                     | (snd p) < (snd q) = p
                     | otherwise         = q

localFrame :: Vec -> (Vec, Vec, Vec)
localFrame n =
    let u = if abs (x n) > eps then Vec (0, 1, 0) else Vec (1, 0, 0)
        v = n `cross` u
    in (u, v, n)

lambert :: Vec -> Vec -> IO (Vec, Double)
lambert ve n = do
    r1 <- randomRIO (0, 1) :: IO Double
    r2 <- randomRIO (0, 1) :: IO Double
    let r2s       = sqrt r2
    let (u, v, w) = localFrame n
    let uu = u `mul` ((cos r1) * r2s)
    let vv = v `mul` ((sin r1) * r2s)
    let ww = n `mul` (sqrt (1.0 - r2))
    let rdir = norm $ uu `add` vv `add` ww
    return (rdir, 1)

reflect :: Vec -> Vec -> IO (Vec, Double)
reflect v n = do
    let rdir = v `sub` (n `mul` (2.0 * n `dot` v))
    return (rdir, 1)

refract :: Vec -> Vec -> Vec -> IO (Vec, Double)
refract v n no = do
    (rdir, _) <- reflect v n
    let into = (n `dot` no) > 0
    let nnt  = if into then (nc / nt) else (nt / nc)
    let ddn  = v `dot` no
    let cos2t = 1.0 - nnt * nnt * (1.0 - ddn * ddn)
    let tdir = norm $ ((v `mul` nnt) `sub`) $ n `mul` ((if into then 1 else -1) * (ddn * nnt + (sqrt cos2t)))
    let a = nt - nc
    let b = nt + nc
    let r0 = (a * a) / (b * b)
    let c = 1.0 - (if into then -ddn else (tdir `dot` n))
    let re = r0 + (1 - r0) * (c ** 5)
    let tr = 1.0 - re
    let pp = 0.25 + 0.5 * re
    pr <- randomRIO (0, 1) :: IO Double
    if cos2t < 0.0 then return (rdir, 1.0)
                   else if pr < pp then return (rdir, (re / pr))
                                   else return (tdir, (tr / (1.0 - pr)))

radiance :: [Sphere] -> Ray -> Int -> IO Vec
radiance scene ray depth = do
    let (i, t) = (isectWithScene scene ray)
    if t == inf then return (Vec (0, 0, 0))
                else (radiance2 scene (scene !! i) ray t depth)

radiance2 :: [Sphere] -> Sphere -> Ray -> Double -> Int -> IO Vec
radiance2 scene obj ray t depth = do
    let f = (col obj)
    let d = (dir ray)
    let x = (org ray) `add` (d `mul` t)
    let n = norm $ x `sub` (pos obj)
    let no = if (d `dot` n) < 0.0  then n else (n `mul` (-1))
    (refv, pdf) <- case (refl obj) of
        Diff -> (lambert d n)
        Spec -> (reflect d n)
        Refr -> (refract d n no)
    nextRad <- (radiance scene (Ray (x, refv)) (succ depth))
    if depth >= 6 then return (Vec (0.0, 0.0, 0.0))
                  else return ((emit obj) `add` ((f `mult` nextRad)) `mul` (1/pdf))

convert :: [IO Vec] -> IO [Vec]
convert [] = do (return [])
convert (x:xs) = do
    xv <- x
    xsv <- convert xs
    return (xv : xsv)

toByte :: Double -> DWord.Word8
toByte x = truncate (((clamp x) ** (1.0 / 2.2)) * 255.0) :: DWord.Word8

main = do
    let w = 400
    let h = 300
    let cam = Ray (Vec (50, 52, 295.6), (norm $ Vec (0, -0.042612, -1)));
    let cx  = Vec (w * 0.5135 / h, 0.0, 0.0)
    let cy  = (norm $ cx `cross` (dir cam)) `mul` 0.5135
    let dirs = map (\(y, x) -> (dir cam) `add` (cy `mul` (y / h - 0.5)) `add` (cx `mul` (x / w - 0.5))) (cartProd [h-1,h-2..0] [0..w-1])
    let rays = map (\d -> Ray ((org cam) `add` (d `mul` 140.0), (norm d))) dirs
    pixels1 <- convert $ map (\r -> (radiance sph r 0)) rays
    pixels2 <- convert $ map (\r -> (radiance sph r 0)) rays
    let pixelData1 = foldr (\col lst -> [(x col), (y col), (z col)] ++ lst) [] pixels1
    let pixelData2 = foldr (\col lst -> [(x col), (y col), (z col)] ++ lst) [] pixels2
    let pixelData  = map toByte $ zipWith (\a b -> ((a + b) / 2)) pixelData1 pixelData2
    let pixelBytes = DVector.fromList pixelData :: DVector.Vector DWord.Word8
    let img = Image { imageHeight = truncate h, imageWidth = truncate w, imageData = pixelBytes } :: Image PixelRGB8
    writePng "image.png" img

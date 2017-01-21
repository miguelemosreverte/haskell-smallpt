
{-# LANGUAGE BangPatterns #-}

import System.Environment
import Control.Monad
import Control.Monad.State (State, evalState, state)
import Control.Applicative
import Codec.Picture
import Data.Time
import Data.Functor
import qualified Data.Word as W
import qualified Data.Vector.Storable as V
import System.Random.Mersenne.Pure64
import Debug.Trace
import Parser

-- Vec
data Vec = Vec (Double, Double, Double) deriving (Show, Eq)
instance (Num Vec) where
    (Vec (x, y, z)) + (Vec (a, b, c)) = Vec (x + a, y + b, z + c)
    (Vec (x, y, z)) - (Vec (a, b, c)) = Vec (x - a, y - b, z - c)
    (Vec (x, y, z)) * (Vec (a, b, c)) = Vec (x * a, y * b, z * c)
    abs = undefined
    signum = undefined
    fromInteger x = Vec (dx, dx, dx) where dx = fromIntegral x
x (Vec (x, _, _)) = x
y (Vec (_, y, _)) = y
z (Vec (_, _, z)) = z
mul (Vec (x, y, z)) s = Vec (x * s, y * s, z * s)
dot (Vec (x, y, z)) (Vec (a, b, c))  = x * a + y * b + z * c
norm (Vec (x, y, z)) = Vec (x * invnrm, y * invnrm, z * invnrm)
    where invnrm = 1 / sqrt (x * x + y * y + z * z)
cross (Vec (x, y, z)) (Vec (a, b, c)) = Vec (y * c - b * z, z * a - c * x, x * b - a * y)
thr (a,b,c) = c

-- Ray
data Ray = Ray (Vec, Vec) deriving (Show)
type Camera = Ray
org (Ray (org, _)) = org
dir (Ray (_, dir)) = dir

-- Material
data Refl = Diff | Spec | Refr deriving (Show, Eq)


type TangentSpace = (Vec,Vec,Vec)

-- Triangle object used for triangle meshes
data Vertex = Vertex { vertPosition :: {-# UNPACK #-} !Vec,
                       vertUV :: {-# UNPACK #-} !Vec,
                       vertTangentSpace :: {-# UNPACK #-} !TangentSpace } deriving (Show, Eq)
data Triangle = Triangle { vertices :: ![Vertex], plane :: !Primitive, halfPlanes :: ![Primitive] } deriving (Show, Eq)


--data Sphere = Sphere (Double, Vec, Vec, Vec, Refl) deriving (Show)
data Primitive = Sphere (Double, Vec, Vec, Vec, Refl)
  | Plane { planeTangentSpace :: {-# UNPACK #-} !TangentSpace, planeDistance :: {-# UNPACK #-} !Double }
  | EasyTriangle (Vec,Vec,Vec, Vec,Vec,Vec,Refl) deriving (Show, Eq)

rad  (Sphere (rad, _, _, _, _   )) = rad
pos  (Sphere (_  , p, _, _, _   )) = p
pos  (EasyTriangle (_  ,_  ,_  , p, _, _, _   )) = p
emit (Sphere (_  , _, e, _, _   )) = e
emit  (EasyTriangle (_  ,_  ,_  , _, e, _, _   )) = e
col  (Sphere (_  , _, _, c, _   )) = c
col  (EasyTriangle (_  ,_  ,_  , _, _, c, _   )) = c
refl (Sphere (_  , _, _, _, refl)) = refl
refl  (EasyTriangle (_  ,_  ,_  , _, _, _, refl   )) = refl

--https://github.com/bkach/HaskellRaycaster/commit/fdb604bf6b4bab6f9b51e19d8b56ef80a7db4f34
-- Given a ray and a distance, produce the point along the ray
pointAlongRay :: Ray -> Double -> Vec
pointAlongRay ray distance = org ray + (dir ray `mul` distance)

{-
--http://tomhammersley.blogspot.com.ar/2011/04/ray-triangle-intersection-in-haskell.html
distanceToPlane :: Primitive -> Vec -> Double
distanceToPlane (Plane !norm !dist) !pos = pos `dot` norm + dist
distanceToPlane _ _ = undefined-}

-- Distance to a plane
distanceToPlane :: Primitive -> Vec -> Double
distanceToPlane (Plane !(_, _, norm) !dist) !pos = (pos `dot` norm) + dist
distanceToPlane _ _ = error "distanceToPlane: Unsupported primitive for this function"

--cocodrile-0.1.2/app/src/Primitive.hs
shapeClosestIntersect :: Primitive -> Ray -> Maybe Double
-- This function intersects a ray with a plane and returns the closest intercept
shapeClosestIntersect (Plane !(_, _, planeNormal) !planeD) (Ray (!rayOrg, !rayDir))
    | dirDotNormal == 0 = Nothing
    | intercept >= 0 = Just intercept
    | otherwise = Nothing
    where !dirDotNormal = rayDir `dot` planeNormal
          !intercept = ((-planeD) - (rayOrg `dot` planeNormal)) / dirDotNormal

--http://tomhammersley.blogspot.com.ar/2011/04/ray-triangle-intersection-in-haskell.html
pointInsideTriangle :: Triangle -> Vec -> Bool
pointInsideTriangle !tri !point = foldr (&&) True $ map (\pln -> (distanceToPlane pln point) >= 0) (halfPlanes tri)

-- Surface normal for 3 points
surfaceNormal :: Vec -> Vec -> Vec -> Vec
surfaceNormal !v1 !v2 !v3 = (v2 - v1) `cross` (v3 - v1)
-- Make a plane
makePlane :: Vec -> Vec -> Vec -> Primitive
makePlane !v1 !v2 !v3 = Plane (tangent, binormal, normal) (-(v1 `dot` normal))
    where
      !normal = norm (surfaceNormal v1 v2 v3)
      !tangent = norm (v2 - v1)
      !binormal = norm (v3 - v1)
{-
 , EasyTriangle (Vec(0,0,50), Vec(0,60,100), Vec (60,0, 50),
  Vec (73, 16.5, 78),Vec (0.0, 0.0, 0.0), Vec (0.25, 0.25, 0.75), Diff)   -- Blue Triangle
  -}
changeVec :: Vec -> Vec
changeVec vec = vec `mul`10 + Vec(0,50,-25)

makeEasyTriangleInterface::((Double,Double,Double),(Double,Double,Double),(Double,Double,Double))->Primitive
makeEasyTriangleInterface(v0,v1,v2) = EasyTriangle(changeVec (Vec(v0)), changeVec (Vec(v1)),changeVec (Vec(v2)),
  Vec (73, 16.5, 78),Vec (0,0,0), Vec (1,1,1)`mul`0.999, Diff)

{--
makeEasyTriangleInterface::((Double,Double,Double),(Double,Double,Double),(Double,Double,Double))->Primitive
makeEasyTriangleInterface(v0,v1,v2) =
  Sphere (15,  Vec(27.0, 16.5, 47.0), Vec (12, 12, 12),    Vec (0, 0, 0),           Diff)
--}
-- Make a triangle
makeTriangle :: Vec -> Vec -> Vec -> Triangle
makeTriangle !v1 !v2 !v3 = Triangle verts newPlane newHalfPlanes
    where newPlane = makePlane v1 v2 v3
          newTanSpace = planeTangentSpace newPlane
          verts = map (\v -> Vertex v (Vec(0,0,0)) newTanSpace) [v1, v2, v3]
          edgeVertices = [v1, v2, v3]
          edges = map norm [v2 - v1, v3 - v2, v1 - v3]
          edgeNormals = map (\edge -> norm $ thr newTanSpace `cross` edge) edges
          -- TODO - The two vectors passed here are just dummies but they can fairly easily be derived
          newHalfPlanes = zipWith (\edgeNormal edgeVertex -> Plane (Vec(0, 0, 1), Vec(0, 1, 0), edgeNormal) (-(edgeNormal `dot` edgeVertex))) edgeNormals edgeVertices


--http://tomhammersley.blogspot.com.ar/2011/04/ray-triangle-intersection-in-haskell.html
intersectRayTriangle :: Triangle -> Ray -> Maybe Double
intersectRayTriangle !triangle !ray =
    case shapeClosestIntersect (plane triangle) ray of
                    Nothing -> Nothing
                    Just (dist') -> if pointInsideTriangle triangle (pointAlongRay ray dist')
                                       then Just (dist')
                                       else Nothing

intersect :: Primitive -> Ray -> Maybe Double
intersect tri@(EasyTriangle (v0,v1,v2, _, _, _, _)) ray =
  intersectRayTriangle (makeTriangle v0 v1 v2) ray

intersect sph@(Sphere (rad, pos, _, _, _)) ray@(Ray (org, dir)) =
    if det < 0.0 then Nothing else f t1 t2
    where op    = pos - org
          b     = op `dot` dir
          det   = b * b - (op `dot` op) + (rad * rad)
          sqdet = sqrt det
          eps   = 1.0e-4
          t1    = b - sqdet
          t2    = b + sqdet
          f a b = if a > eps then Just a else if b > eps then Just b else Nothing

-- Scene
type Scene = [Primitive]
sph :: Scene
sph = [ Sphere (1e5,  Vec ( 1e5+1,  40.8, 81.6),    Vec (0.0, 0.0, 0.0), Vec (0.75, 0.25, 0.25),  Diff)   -- Left
      , Sphere (1e5,  Vec (-1e5+99, 40.8, 81.6),    Vec (0.0, 0.0, 0.0), Vec (0.25, 0.25, 0.75),  Diff)   -- Right
      , Sphere (1e5,  Vec (50.0, 40.8,  1e5),       Vec (0.0, 0.0, 0.0), Vec (0.75, 0.75, 0.75),  Diff)   -- Back
      , Sphere (1e5,  Vec (50.0, 40.8, -1e5+170),   Vec (0.0, 0.0, 0.0), Vec (0.0, 0.0, 0.0),     Diff)   -- Front
      , Sphere (1e5,  Vec (50, 1e5, 81.6),          Vec (0.0, 0.0, 0.0), Vec (0.75, 0.75, 0.75),  Diff)   -- Bottom
      , Sphere (1e5,  Vec (50,-1e5+81.6,81.6),      Vec (0.0, 0.0, 0.0), Vec (0.75, 0.75, 0.75),  Diff)   -- Top
      --, Sphere (16.5, Vec (27, 16.5, 47),           Vec (0.0, 0.0, 0.0), Vec (1,1,1) `mul` 0.999, Spec)   -- Mirror
      --, Sphere (16.5, Vec (73, 16.5, 78),Vec (0.0, 0.0, 0.0), Vec (1,1,1) `mul` 0.999, Refr)   -- Glass
      , Sphere (600,  Vec (50, 681.6 - 0.27, 81.6), Vec (12, 12, 12),    Vec (0, 0, 0),           Diff) ] -- Light

-- Utility functions
clamp :: Double -> Double
clamp = (max 0.0) . (min 1.0)

intersects :: Scene -> Ray -> (Maybe Double, Int)
intersects scene ray = if null lst then (Nothing, undefined) else minimum lst
    where lst = filter (not . null . fst) $ zip [ intersect obj ray | obj <- scene ] [0..]

nextDouble :: State PureMT Double
nextDouble = state $ randomDouble

radiance :: Scene -> Ray -> Int -> State PureMT Vec
radiance scene ray depth = case (intersects scene ray) of
    (Nothing, _) -> return $ Vec (0.0,0.0,0.0)
    (Just t,  i) -> do
        r0 <- nextDouble
        r1 <- nextDouble
        r2 <- nextDouble
        let obj  = (scene !! i)
        let c    = col obj
        let prob = (max (x c) (max (y c) (z c)))
        if depth >= 5 && r0 >= prob
            then return (emit obj)
            else do
                let rlt = if depth < 5 then 1 else prob
                let f = (col obj)
                let d = (dir ray)
                let p = (org ray) + (d `mul` t)
                let n = norm $ p - (pos obj)
                let nl = if (d `dot` n) < 0.0  then n else (-n)
                nextRad <- case (refl obj) of
                    Diff -> (radiance scene (Ray (p, ndir)) (succ depth))
                        where th  = 2.0 * pi * r1
                              r2s = sqrt r2
                              w = nl
                              u = norm $ (if (abs (x w)) > 0.1 then Vec (0, 1, 0) else Vec (1, 0, 0)) `cross` w
                              v = w `cross` u
                              uu = u `mul` ((cos th) * r2s)
                              vv = v `mul` ((sin th) * r2s)
                              ww = w `mul` (sqrt (1.0 - r2))
                              ndir = norm (uu + vv + ww)

                    Spec -> (radiance scene (Ray (p, ndir)) (succ depth))
                        where ndir = d - (nl `mul` (2.0 * nl `dot` d))

                    Refr -> if cos2t < 0.0
                                then (radiance scene (Ray (p, rdir)) (succ depth))
                                else if depth > 2
                                         then if r1 < pp
                                                  then fmap (`mul` (re / pp)) (radiance scene (Ray (p, rdir)) (succ depth))
                                                  else fmap (`mul` (tr / (1.0 - pp))) (radiance scene (Ray (p, tdir)) (succ depth))
                                         else (\r t -> (r `mul` re) + (t `mul` tr))
                                              <$> (radiance scene (Ray (p, rdir)) (succ depth))
                                              <*> (radiance scene (Ray (p, tdir)) (succ depth))
                            where rdir  = d - (nl `mul` (2.0 * nl `dot` d))
                                  into  = (n `dot` nl) > 0
                                  nnt   = if into then (nc / nt) else (nt / nc)
                                  ddn   = d `dot` nl
                                  cos2t = 1.0 - nnt * nnt * (1.0 - ddn * ddn)
                                  tdir  = norm $ ((d `mul` nnt) -) $ n `mul` ((if into then 1 else -1) * (ddn * nnt + (sqrt cos2t)))
                                  nc    = 1.0
                                  nt    = 1.5
                                  a     = nt - nc
                                  b     = nt + nc
                                  r0    = (a * a) / (b * b)
                                  c     = 1.0 - (if into then -ddn else (tdir `dot` n))
                                  re    = r0 + (1 - r0) * (c ** 5)
                                  tr    = 1.0 - re
                                  pp    = 0.25 + 0.5 * re
                return $ (emit obj) + ((f * nextRad) `mul` (1/rlt))

toByte :: Double -> W.Word8
toByte x = truncate (((clamp x) ** (1.0 / 2.2)) * 255.0) :: W.Word8

subsample :: State PureMT (Double, Double)
subsample = do
    r1 <- fmap (* 2) nextDouble
    r2 <- fmap (* 2) nextDouble
    let dx = if r1 < 1 then (sqrt r1) - 1 else 1 - (sqrt (2 - r1))
    let dy = if r2 < 1 then (sqrt r2) - 1 else 1 - (sqrt (2 - r2))
    return $ (dx, dy)

tracePath :: Scene -> Camera -> Double -> Double -> Double -> Double -> Int -> State PureMT Vec
tracePath scene cam x y w h spp = do
    let cx  = Vec (w * 0.5135 / h, 0.0, 0.0)
    let cy  = (norm $ cx `cross` (dir cam)) `mul` 0.5135

    del <- sequence (take 4 (repeat subsample))
    let sub = [ (dx, dy) | dx <- [0..1], dy <- [0..1] ]
    let crd = take 4 (repeat (x, y))
    let pos = [ (((sx + 0.5 + dx) / 2 + x) / w - 0.5, ((sy + 0.5 + dy) / 2 + y) / h - 0.5) | ((x, y), (sx, sy), (dx, dy)) <- zip3 crd sub del ]
    let dirs = [ (dir cam) + cx `mul` xx + cy `mul` yy | (xx, yy) <- pos ]
    let rays = [ Ray ((org cam) + (d `mul` 140.0), (norm d)) | d <- dirs ]
    pixels <- sequence [ (radiance scene r 0) | r <- (foldr1 (++) $ take spp $ repeat rays) ]
    return $ (foldr1 (+) pixels) `mul` (1 / (4 * fromIntegral spp))


main :: IO ()
main = do

  args <- getArgs
  let argc = length args
  let w   = if argc >= 1 then (read (args !! 0)) else 400 :: Int
  let h   = if argc >= 2 then (read (args !! 1)) else 300 :: Int
  let spp = if argc >= 3 then (read (args !! 2)) else 4   :: Int

  startTime <- getCurrentTime

  putStrLn "--- haskell-smallpt ---"
  putStrLn " usage: ./haskell-smallpt [width] [height] [samples]"
  putStrLn $ "  width = " ++ (show w)
  putStrLn $ " height = " ++ (show h)
  putStrLn $ "    spp = " ++ (show spp)

  let ww = fromIntegral w :: Double
  let hh = fromIntegral h :: Double

  a <-getTrianglesConstructorsFromData "asd" "ad"

  print $ show(a)

  let b = map makeEasyTriangleInterface a
  let xxx = sph ++ b

  --print $ show(b)

  gen <- newPureMT


  let cam = Ray (Vec (50, 52, 295.6), (norm $ Vec (0, -0.042612, -1)));
  let pixels = evalState (sequence $ [ (tracePath xxx cam x y ww hh spp) | y <- [hh-1,hh-2..0], x <- [0..ww-1] ]) gen
  let pixelData = map toByte $ (foldr (\col lst -> [(x col), (y col), (z col)] ++ lst) [] pixels)
  let pixelBytes = V.fromList pixelData :: V.Vector W.Word8
  let img = Image { imageHeight = h, imageWidth = w, imageData = pixelBytes } :: Image PixelRGB8
  writePng "image.png" img

  endTime <- getCurrentTime
  print $ diffUTCTime endTime startTime

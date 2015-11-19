module Main where

import Data.List
import Control.Concurrent
import Control.Monad
import Codec.Picture
import qualified Data.Matrix as M
import qualified Data.Vector as V

type RealNum = Float
type Vector = M.Matrix RealNum
type Point = Vector
type Radius = RealNum
type Transform = M.Matrix RealNum

yellowPixel = PixelRGB8 255 215 0
blackPixel = PixelRGB8 33 33 33

-- | Ray Starting vector, Direction
data Ray = Ray Point Vector deriving (Show, Eq)

data Geometry = Sphere Radius Point

-- | Camera
-- World to camera, cameraToWorld, clipHither, clipYon
data Camera = Camera RealNum RealNum
  deriving (Show)

rayEpsilon :: RealNum
rayEpsilon = 1e-7

a = M.fromList 1 4 [1..4::Float]

toRadians :: RealNum -> RealNum
toRadians deg = deg * pi / 180.0

-- | Vector length (norm)
vlength :: Vector -> RealNum
vlength v = sqrt (sumSqrs v)
  where sumSqrs m = V.foldl (\x y -> x**2 + y) 0 (M.getRow 1 m)

-- | Dot product of vectors
dot :: Vector -> Vector -> RealNum
dot v1 v2 = V.sum $ V.map (\(x,y) -> x*y) (V.zip a b)
  where
    a = M.getRow 1 v1
    b = M.getRow 1 v2

screenWidth :: Int
screenWidth = 200

screenHeight :: Int
screenHeight = 150

maxRaySteps :: Integer
maxRaySteps = 5

defaultCamera = Camera near far
  where
        near = 0.01
        far = 200.0

-- | builds a new vector in homogeneous coordinates
buildVector x y z = M.fromList 4 1 [x,y,z,1]

defaultSphere :: Geometry
defaultSphere = Sphere 3.0 (buildVector 10 0 0)

-- | cameraTranform builds a projection matrix (Camera to World) from a Camera
perspective :: RealNum -> RealNum -> RealNum -> Transform
perspective fov n f = perspectMatrix * eyeCoordScale fov
  where
    perspectMatrix = m
    m = M.fromList 4 4 [1,0,0,0,
                        0,1,0,0,
                        0,0,a,b,
                        0,0,1,0]
    a = f*invDenom
    b = (-f)*n*invDenom
    invDenom = 1.0/(f-n)

-- | Scales to canonical viewing volume
eyeCoordScale :: RealNum -> Transform
eyeCoordScale fov = scaleT v
  where
    v = buildVector i i 1
    i = invTanAng fov

invTanAng :: RealNum -> RealNum
invTanAng fov = 1.0 / tan(toRadians fov  / 2.0)

identityTransform :: Transform
identityTransform = M.identity 4

normalize :: Vector -> Vector
normalize v = M.fromList 4 1 [x/l, y/l, z/l, 1]
  where
  x = M.getElem 1 1 v
  y = M.getElem 2 1 v
  z = M.getElem 3 1 v
  w = M.getElem 4 1 v
  l = sqrt (x**2 + y**2 + z**2 + w**2)


scaleT :: Vector -> Transform
scaleT v = M.fromList 4 4 [x,0,0,0,
                           0,y,0,0,
                           0,0,z,0,
                           0,0,0,1]
           where
             x = M.getElem 1 1 v
             y = M.getElem 2 1 v
             z = M.getElem 3 1 v

translateT :: Vector -> Transform
translateT v = m + identityTransform
  where
    m = M.fromList 4 4 [0,0,0,x,
                        0,0,0,y,
                        0,0,0,z,
                        0,0,0,0]
    x = M.getElem 1 1 v
    y = M.getElem 2 1 v
    z = M.getElem 3 1 v

-- samples = [M.fromList 4 1 [x, y, 0, 0] | x <- [0..screenWidth], y <- [0..screenHeight]]

-- | Squares a vector
squareVector :: Vector -> RealNum
squareVector v = M.getElem 1 1 (M.transpose a * a)

quadratic :: RealNum -> RealNum -> RealNum -> RealNum
quadratic a b c =
  b**2 - 4*a*c



intersects :: Ray -> Geometry -> Bool
intersects (Ray o v) (Sphere radius p) =
  discriminant >= 0
  where
    discriminant = quadratic a b c
    a = dot v v
    b = 2 * dot os v
    c = dot os os - radius * 2
    os = p - o

distance :: Ray -> Geometry -> RealNum
distance (Ray o v) (Sphere radius p)
  = (-b + sqrt discriminant) / (2*a)
  where
    discriminant = quadratic a b c
    a = dot v v
    b = 2 * dot os v
    c = dot os os - radius ** 2
    os = p - o

rayTrace :: Int -> Int -> PixelRGB8
rayTrace x y =
  if intersects r defaultSphere then
    PixelRGB8 (round (5*d)) 128 0
  else
    blackPixel
  where
    r = generateRay x y defaultCamera
    d = distance r defaultSphere

generateRay :: Int -> Int -> Camera -> Ray
generateRay x y c = Ray origin direction
  where
    origin = buildVector 0 0 0
    direction = normalize $ perspective 55.0 0.1 50.0  * buildVector ndx ndy 0
    --ndx = 2*(fromIntegral x / fromIntegral screenWidth) - 1
    --ndy = 2*(fromIntegral y / fromIntegral screenHeight) - 1
    ndx = (fromIntegral x / fromIntegral screenWidth)
    ndy = (fromIntegral y / fromIntegral screenHeight)


main :: IO ()
main =
  writePng "image.png" ( generateImage rayTrace screenWidth screenHeight)

module Main where

-- import Data.List
import Control.Concurrent
import Control.Monad
import Codec.Picture
import Numeric.Matrix

type Vector = Matrix Double
type Point = Vector
type Radius = Double
type Transform = Matrix Double

yellowPixel = PixelRGB8 255 215 0
blackPixel = PixelRGB8 33 33 33

-- | Ray Starting vector, Direction
data Ray = Ray Point Vector deriving (Show, Eq)

data Geometry = Sphere Radius Point

-- | Camera
-- World to camera, cameraToWorld, clipHither, clipYon
data Camera = Camera Transform Double Double
  deriving (Show)

defaultCamera = Camera (unit 4) 0.01 100.0

rayEpsilon :: Double
rayEpsilon = 1e-7

toRadians :: Double -> Double
toRadians deg = deg * pi / 180.0

-- | Vector length (norm)
vlength :: Vector -> Double
vlength v = sqrt sumsquares
            where
              sumsquares = foldr (\x y -> x**2 + y) 0 (col 1 v) 

-- | Dot product of vectors
dot :: Vector -> Vector -> Double
dot v1 v2 = sum $ zipWith (*) (col 1 v1) (col 1 v2)

screenWidth :: Int
screenWidth = 200

screenHeight :: Int
screenHeight = 150

maxRaySteps :: Integer
maxRaySteps = 5

              
-- | builds a new vector in homogeneous coordinates
buildVector :: Double -> Double -> Double -> Matrix Double
buildVector x y z = transpose (fromList [[x,y,z,1]])

defaultSphere :: Geometry
defaultSphere = Sphere 3.0 (buildVector 10.0 0 0)

-- | cameraTranform builds a projection matrix (Camera to World) from a Camera
perspective :: Double -> Double -> Double -> Transform
perspective fov n f = perspectMatrix * eyeCoordScale fov
    where
      perspectMatrix = m
      m = fromList [[1,0,0,0],
                    [0,1,0,0],
                    [0,0,a,b],
                    [0,0,1,0]]
      a = f*invDenom
      b = (-f)*n*invDenom
      invDenom = 1.0/(f-n)

-- | Scales to canonical viewing volume
eyeCoordScale :: Double -> Transform
eyeCoordScale fov = scaleT v
  where
    v = buildVector i i 1
    i = invTanAng fov

invTanAng :: Double -> Double
invTanAng fov = 1.0 / tan(toRadians fov  / 2.0)

identityTransform :: Transform
identityTransform = unit 4

normalize :: Vector -> Vector
normalize v = Numeric.Matrix.map (\x -> x/l) v
    where
      l = vlength v

vectorMultiply :: Transform -> Vector -> Vector
vectorMultiply m v = transpose (m * transpose v)


scaleT :: Vector -> Transform
scaleT v = diag (col 1 v)

a = buildVector 3.0 4.0 5.0

myscale = scaleT (buildVector 5.0 10.0 15.0)

-- translateT :: Vector -> Transform
-- translateT v = m + identityTransform
--   where
--     m = M.fromList 4 4 [0,0,0,x,
--                         0,0,0,y,
--                         0,0,0,z,
--                         0,0,0,0]
--     x = M.getElem 1 1 v
--     y = M.getElem 2 1 v
--     z = M.getElem 3 1 v

-- -- samples = [M.fromList 4 1 [x, y, 0, 0] |
-- -- x <- [0..screenWidth], y <- [0..screenHeight]]

-- -- | Squares a vector
-- squareVector :: Vector -> RealNum
-- squareVector v = M.getElem 1 1 (M.transpose a * a)

quadratic :: Double -> Double -> Double -> Double
quadratic a b c =
  b**2 - 4*a*c

-- | Determines a ray geometry intersection
intersects :: Ray -> Geometry -> Bool
intersects (Ray o v) (Sphere radius p) =
  discriminant >= 0
  where
    discriminant = quadratic a b c
    a = dot v v
    b = 2 * dot os v
    c = dot os os - radius * 2
    os = p - o

distance :: Ray -> Geometry -> Double
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
    direction = normalize $ vectorMultiply perspTransform rasterCoords
    --ndx = 2*(fromIntegral x / fromIntegral screenWidth) - 1
    --ndy = 2*(fromIntegral y / fromIntegral screenHeight) - 1
    perspTransform = perspective 55.0 0.1 50.0
    rasterCoords = buildVector ndx ndy 0.0
    ndx = (fromIntegral x / fromIntegral screenWidth)
    ndy = (fromIntegral y / fromIntegral screenHeight)


main :: IO ()
main =
  writePng "image.png" ( generateImage rayTrace screenWidth screenHeight)

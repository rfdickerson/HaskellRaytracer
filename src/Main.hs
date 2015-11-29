{-# LANGUAGE Haskell2010, TemplateHaskell, QuasiQuotes #-}

module Main where

-- import Data.List
import Control.Concurrent
import Control.Monad
import Codec.Picture

-- import Numeric.Matrix

data Matrix4 = Matrix4
               Double Double Double Double
               Double Double Double Double
               Double Double Double Double
               Double Double Double Double
               deriving (Show, Eq)


multMat4 :: Matrix4 -> Matrix4 -> Matrix4
multMat4 (Matrix4 m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33)
  (Matrix4 n00 n01 n02 n03 n10 n11 n12 n13 n20 n21 n22 n23 n30 n31 n32 n33)
  = Matrix4 j00 j01 j02 j03 j10 j11 j12 j13 j20 j21 j22 j23 j30 j31 j32 j33
    where
      j00 = m00*n00 + m01*n10 + m02*n20 + m03*n30
      j01 = m00*n01 + m01*n11 + m02*n21 + m03*n31
      j02 = m00*n02 + m01*n12 + m02*n22 + m03*n32
      j03 = m00*n03 + m01*n13 + m02*n23 + m03*n33 -- end
      j10 = m10*n00 + m11*n10 + m12*n20 + m13*n30
      j11 = m10*n01 + m11*n11 + m12*n21 + m13*n31
      j12 = m10*n02 + m11*n12 + m12*n22 + m13*n32
      j13 = m10*n03 + m11*n13 + m12*n23 + m13*n33 -- end
      j20 = m20*n00 + m21*n10 + m22*n20 + m23*n30
      j21 = m20*n01 + m21*n11 + m22*n21 + m23*n31
      j22 = m20*n02 + m21*n12 + m22*n22 + m23*n32
      j23 = m20*n03 + m21*n13 + m22*n23 + m23*n33 -- end
      j30 = m30*n00 + m31*n10 + m32*n20 + m33*n30
      j31 = m30*n01 + m31*n11 + m32*n21 + m33*n31
      j32 = m30*n02 + m31*n12 + m32*n22 + m33*n32
      j33 = m30*n03 + m31*n13 + m32*n23 + m33*n33

data Vector4 = Vector4
               Double Double Double Double
               deriving (Show, Eq)

vectorMult4 :: Matrix4 -> Vector4 -> Vector4
vectorMult4 (Matrix4 m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33)
  (Vector4 x y z w) =
  Vector4 a b c d
  where
    a = m00 * x + m01 * y + m02 * z + m03 * w
    b = m10 * x + m11 * y + m12 * z + m13 * w
    c = m20 * x + m21 * y + m22 * z + m23 * w
    d = m30 * x + m31 * y + m32 * z + m33 * w


unit4 :: Matrix4
unit4 = Matrix4 1 0 0 0
                0 1 0 0
                0 0 1 0
                0 0 0 1

vlength4 :: Vector4 -> Double
vlength4 (Vector4 x y z w) = sqrt(x**2 + y**2 + z**2 + w**2)

dot4 :: Vector4 -> Vector4 -> Double
dot4 (Vector4 x1 y1 z1 w1) (Vector4 x2 y2 z2 w2) =
  x1*x2 + y1*y2 + z1*z2 + w1*w2

normalize4 :: Vector4 -> Vector4
normalize4 v@(Vector4 x y z w) =
  Vector4 (x*s) (y*s) (z*s) (w*s)
  where
    s = 1/vlength4 v

subVec4 :: Vector4 -> Vector4 -> Vector4
subVec4 (Vector4 x1 y1 z1 w1) (Vector4 x2 y2 z2 w2)
  = Vector4 (x1-x2) (y1-y2) (z1-z2) (w1-w2)

-- type Vector = Matrix Double
type Point = Vector4
type Radius = Double
type Transform = Matrix4

yellowPixel = PixelRGB8 255 215 0
blackPixel = PixelRGB8 33 33 33

-- | Ray Starting vector, Direction
data Ray = Ray Point Vector4 deriving (Show, Eq)

data Geometry = Sphere Radius Point

-- | Camera
-- World to camera, cameraToWorld, clipHither, clipYon
data Camera = Camera Transform Double Double
            deriving (Show)

defaultCamera = Camera (unit4) 0.01 100.0

rayEpsilon :: Double
rayEpsilon = 1e-7

toRadians :: Double -> Double
toRadians deg = deg * pi / 180.0

-- -- | Vector length (norm)
-- vlength :: Vector -> Double
-- vlength v = sqrt sumsquares
--   where
--     sumsquares = foldr (\x y -> x**2 + y) 0 (col 1 v)

-- -- | Dot product of vectors
-- dot :: Vector -> Vector -> Double
-- dot v1 v2 = sum $ zipWith (*) (col 1 v1) (col 1 v2)

screenWidth :: Int
screenWidth = 800

screenHeight :: Int
screenHeight = 600

maxRaySteps :: Integer
maxRaySteps = 5


-- -- | builds a new vector in homogeneous coordinates
-- buildVector :: Double -> Double -> Double -> Matrix Double
-- buildVector x y z = (fromList [[x],[y],[z],[1]])

defaultSphere :: Geometry
defaultSphere = Sphere 0.4 (Vector4 2.0 (2.0) 0 1.0)

-- | cameraTranform builds a projection matrix (Camera to World) from a Camera
perspective :: Double -> Double -> Double -> Transform
perspective fov n f = multMat4 m (eyeCoordScale fov)
  where
    m = Matrix4 1 0 0 0
                0 1 0 0
                0 0 a b
                0 0 1 0
    a = f*invDenom
    b = (-f)*n*invDenom
    invDenom = 1.0/(f-n)

-- | Scales to canonical viewing volume
eyeCoordScale :: Double -> Transform
eyeCoordScale fov = scaleT i i 1
  where
    i = invTanAng fov

invTanAng :: Double -> Double
invTanAng fov = 1.0 / tan(toRadians fov  / 2.0)


identityTransform :: Transform
identityTransform = unit4

-- normalize :: Vector -> Vector
-- normalize v = Numeric.Matrix.map (\x -> x/l) v
--   where
--     l = vlength v

-- vectorMultiply :: Transform -> Vector -> Vector
-- vectorMultiply m v = transpose (m * transpose v)


scaleT :: Double -> Double -> Double -> Transform
scaleT x y z = Matrix4 x 0 0 0
                       0 y 0 0
                       0 0 z 0
                       0 0 0 1

a = Vector4 1.0 4.0 5.0 1.0

myscale = scaleT 5.0 10.0 15.0


translateT :: Double -> Double -> Double -> Transform
translateT x y z = Matrix4 1 0 0 x
                           0 1 0 y
                           0 0 1 z
                           0 0 0 1

-- translateT :: Vector -> Transform
-- translateT v = m
--   where
--   m = fromList [[1,0,0,x],
--                 [0,1,0,y],
--                 [0,0,1,z],
--                 [0,0,0,0]]
--   x = at v (1, 1)
--   y = at v (2, 1)
--   z = at v (3, 1)


discrim :: Double -> Double -> Double -> Double
discrim a b c =
  b**2 - 4*a*c

-- | Determines a ray geometry intersection
intersects :: Ray -> Geometry -> Bool
intersects (Ray o v) (Sphere radius p) =
  discriminant >= 0
  where
    discriminant = discrim a b c
    a = dot4 v v
    b = 2 * dot4 os v
    c = dot4 os os - (radius ** 2)
    os = subVec4 p o

distance :: Ray -> Geometry -> Double
distance (Ray o v) (Sphere radius p)
  = (-b - sqrt discriminant) / (2*a)
  where
    discriminant = discrim a b c
    a = dot4 v v
    b = 2 * dot4 os v
    c = dot4 os os - radius ** 2
    os = subVec4 p o

-- | raytrace converts a raster coordinate and returns a pixel color
rayTrace :: Int -> Int -> PixelRGB8
rayTrace x y =
  if intersects r defaultSphere then
    PixelRGB8 (round (0.2*d)) 128 0
  else
    blackPixel
  where
    r = generateRay x y defaultCamera
    d = distance r defaultSphere

-- | Generates a ray through the camera to the near frustrum
generateRay :: Int -> Int -> Camera -> Ray
generateRay x y c = Ray origin direction
  where
    origin = Vector4 0 0 0 1
    direction = normalize4 ( vectorMult4 perspTransform rasterCoords )
    perspTransform = perspective 55.0 0.1 20.0
    rasterCoords = Vector4 ndx ndy 0.0 1.0
    ndx = fromIntegral x / fromIntegral screenWidth
    ndy = fromIntegral y / fromIntegral screenHeight

-- | rasterizes the image to a file
main :: IO ()
main =
  writePng "image.png" ( generateImage rayTrace screenWidth screenHeight)

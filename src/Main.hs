module Main where

import Data.List
import Control.Concurrent
import Control.Monad
import Control.Concurrent.STM
import qualified Data.Matrix as M
import qualified Data.Vector as V

type RealNum = Float
type Vector = M.Matrix RealNum
type Point = Vector
type Radius = RealNum
type Transform = M.Matrix RealNum
type Bitmap = [Pixel]


data Ray = Ray Point Vector deriving (Show, Eq)
data Geometry = Sphere Radius Point | Triangle

-- | Pixel represents a color, r, g, b
data Pixel = RGB Float Float Float deriving (Show, Eq)
data Image = Image Int Int [Pixel] deriving Show

-- | Camera
-- World to camera, cameraToWorld, clipHither, clipYon
data Camera = Camera Transform Transform RealNum RealNum
  deriving (Show)

--pi :: RealNum
--pi = 3.14159265359


     
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

screenWidth :: RealNum
screenWidth = 800

screenHeight :: RealNum
screenHeight = 600

maxRaySteps :: Integer
maxRaySteps = 5

newVector :: [RealNum] -> Point
newVector x = M.fromList 4 1 x

defaultCamera = Camera (translateT $ newVector [5,0,0]) (translateT $ newVector [-5,0,0]) 0.010 100.0

cameraTransform :: Camera -> Transform
cameraTransform (Camera w2c c2w hither yon) = M.fromList 4 4 [0,0,0,0,0,0,0,0,0,0,a,b,0,0,0,0] + identityTransform
  where a = yon / (yon-hither)
        b = -(yon*hither/(yon-hither))

invTanAng :: RealNum -> RealNum
invTanAng fov = 1.0/tan(toRadians(fov) / 2.0)

-- defaultCamera :: Camera
-- defaultCamera = Camera identityTransform 0.1 100 0 0

identityTransform :: Transform
identityTransform = M.identity 4

scaleT :: Vector -> Transform
scaleT v = M.fromList 4 4 [x,0,0,0,0,y,0,0,0,0,z,0,0,0,0,1]
           where
             x = M.getElem 1 1 v
             y = M.getElem 2 1 v
             z = M.getElem 3 1 v

translateT :: Vector -> Transform
translateT v = identityTransform + (M.fromList 4 4 t)
  where
    t = [0,0,0] ++ [M.getElem 1 1 v]
      ++ [0,0,0] ++ [M.getElem 1 2 v]
      ++ [0,0,0] ++ [M.getElem 1 3 v]

samples = [M.fromList 4 1 [x, y, 0, 0] | x <- [0..screenWidth], y <- [0..screenHeight]]

generateRay :: Camera -> Ray -> Ray
generateRay (Camera t _ _ _ ) (Ray o r) = Ray o (t * r)

-- rayCast :: Ray -> Shape -> Bool
-- rayCast r (Sphere Radius Point)

raster :: Image -> String
raster (Image width height bitmap) = join "\n" line_list
                                     where line_list = ["PS3", show width ++ " " ++ show height,
                                                        "255", map (join " " . map show) bitmap]

saveImage :: Image -> IO ()
saveImage img = do
   writeFile "image.pgm" (raster img)

sayHi :: IO ()
sayHi = do
  putStrLn "Hi!!"

main :: IO ()
main =
  putStrLn "Hello"

--dot (Vector x1 y1 z1) (Vector x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

--cross :: Vector -> Vector -> Vector
--cross (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector x y z
--                                            where
--                                              x = y1 * z2 - z1 * y2
--                                              y = z1 * x2 - x1 * z2
--                                              z = x1 * y2 - y1 * x2


-- intersects :: Ray -> Shape -> Bool
-- insersects (Circle Radius Point) =
--  let e1 = p2 - p1

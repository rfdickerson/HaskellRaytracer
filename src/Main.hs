module Main where

import Data.List
import qualified Data.Matrix as M
import qualified Data.Vector as V

type RealNum = Float
type Vector = M.Matrix RealNum
type Point = Vector
type Radius = RealNum
type Transform = M.Matrix RealNum

data Ray = Ray Point Vector
data Geometry = Sphere Radius Point | Triangle

-- | Camera
-- Position, view direction, aspect ratio, up direction
data Camera = Camera Point Vector RealNum Vector
  deriving (Show)

rayEpsilon :: RealNum
rayEpsilon = 1e-7

a = M.fromList 1 4 [1..4::Float]

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

defaultCamera = Camera (newVector [5,0,0]) (newVector [-1,0,0]) 1.0 (newVector [0,1,0])

cameraTransform :: Camera -> Transform
cameraTransform c = identityTransform

-- defaultCamera :: Camera
-- defaultCamera = Camera identityTransform 0.1 100 0 0

identityTransform :: Transform
identityTransform = M.identity 4

translateT :: Vector -> Transform
translateT v = (M.fromList 4 4 t) + identityTransform
  where
    t = [0,0,0] ++ [M.getElem 1 1 v] ++ [0,0,0] ++ [M.getElem 2 1 v] ++ [0,0,0] ++ [M.getElem 3 1 v] ++ [0,0,0,0]

samples = [M.fromList 4 1 [x, y, 0, 0] | x <- [0..screenWidth], y <- [0..screenHeight]]

generateRay :: Camera -> Ray -> Ray
generateRay (Camera t _ _ _ ) (Ray o r) = Ray o (t * r)

-- rayCast :: Ray -> Shape -> Bool
-- rayCast r (Sphere Radius Point)

saveImage :: String -> IO ()
saveImage img = do
   writeFile "image.pgm" img

main :: IO ()
main = do
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

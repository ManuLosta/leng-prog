module Shape where

data Point = Point {x :: Double, y :: Double} deriving (Eq, Show)

data Circle = Circle Point Double deriving (Eq, Show)

data Rectangle = Rectangle Point Point deriving (Eq, Show)

-- A point from a tuple Pair
point :: (Double, Double) -> Point
point (a, b) = Point {x = a, y = b}

-- The origin
origin :: Point
origin = Point {x = 0, y = 0}

-- Rectangle from a Tuple where (x0 y0) == origin
rectangle :: (Double, Double) -> Rectangle
rectangle (a, b) = Rectangle origin (point (a, b))

base :: Rectangle -> Double
base (Rectangle p1 p2) = abs (x p1 - x p2)

height :: Rectangle -> Double
height (Rectangle p1 p2) = abs (y p1 - y p2)

-- Circle from radius
circle :: Double -> Circle
circle = Circle origin

-- Clase Shift

class Shift a where
  shift :: a -> (Double, Double) -> a

instance Shift Point where
  shift p (x2, y2) = Point {x = x p + x2, y = y p + y2}

instance Shift Rectangle where
  shift (Rectangle o p) (x2, y2) = Rectangle (shift o (x2, y2)) (shift p (x2, y2))

instance Shift Circle where
  shift (Circle o r) (x2, y2) = Circle (shift o (x2, y2)) r

-- Define the Surface class

class Surface a where
  surface :: a -> Double

instance Surface Rectangle where
  surface rect = base rect * height rect

instance Surface Circle where
  surface (Circle _ r) = pi * r ^ 2

module Lib
    ( someFunc
    ) where

data Loc = Point Int Int deriving (Show, Eq)
type Space = [Loc]
type Depth = Int

adjacentPoints :: Loc -> Space
adjacentPoints (Point x y) = [
    (Point (x+1) (y+1)),
    (Point x (y+1)),
    (Point (x-1) (y+1)),
    (Point (x+1) y),
    (Point (x-1) y),
    (Point (x+1) (y-1)),
    (Point x (y-1)),
    (Point (x-1) (y-1))]

nearbyPoints :: Loc -> Depth -> Space -- need to optimize this shit with memoization
nearbyPoints loc 0 = []
nearbyPoints loc depth = loc : (concat $ map (\x -> nearbyPoints x (depth-1)) (adjacentPoints loc))

someFunc :: IO ()
someFunc = do
    let moves = nearbyPoints (Point 0 0) 3
    mapM_ (putStrLn . show) moves
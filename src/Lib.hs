{-# LANGUAGE RecordWildCards #-}

module Lib
    ( someFunc
    ) where

import Data.List

data Entity = Entity {
    symbol :: Char,
    name :: String,
    loc :: Loc,
    control :: Space
} deriving (Show, Eq)

data Loc = Point Int Int deriving (Show, Eq)

type Space = [Loc]

type Depth = Int

data World = World {
    steps :: Int,
    entities :: [Entity]
} deriving (Show, Eq)

player :: Entity
player = Entity {
    symbol = 'P', 
    name = "Player", 
    loc = Point 0 0 , 
    control = adjacentPoints (Point 0 0)
    }

initializeWorld :: World
initializeWorld = World { steps = 0, entities = []}

spawn :: World -> Entity -> World
spawn World{..} e = World { entities = e : entities, .. }

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

nearbyPoints :: Loc -> Depth -> Space
nearbyPoints p 0 = []
nearbyPoints p 1 = adjacentPoints p
nearbyPoints p n = union (adjacentPoints p) (concatMap adjacentPoints (nearbyPoints p (n-1)))

printPoints :: Space -> IO ()
printPoints s = mapM_ (putStrLn . show) s

someFunc :: IO ()
someFunc = do
    let p = nearbyPoints (Point 0 0) 5
    printPoints p
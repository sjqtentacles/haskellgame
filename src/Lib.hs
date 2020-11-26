{-# LANGUAGE RecordWildCards #-}

module Lib
    ( someFunc
    ) where

import Data.List

data Point a = Point a a deriving (Show, Eq)
type Loc = Point Int
type Space = [Loc]
type Depth = Int

data Entity = Entity {
    symbol :: Char,
    name :: String,
    loc :: Loc,
    control :: Space
} deriving (Show, Eq)

data World = World {
    steps :: Int,
    entities :: [Entity]
} deriving (Show, Eq)

genesis :: World
genesis = World { steps = 0, entities = []}

spawn :: World -> Entity -> World
spawn World{..} e = World { entities = e : entities, .. }

adjacentPoints :: Loc -> Space
adjacentPoints (Point x y) = [
    Point (x+1) (y+1),
    Point x (y+1),
    Point (x-1) (y+1),
    Point (x+1) y,
    Point (x-1) y,
    Point (x+1) (y-1),
    Point x (y-1),
    Point (x-1) (y-1)]

nearbyPoints :: Loc -> Depth -> Space
nearbyPoints p 0 = []
nearbyPoints p 1 = adjacentPoints p
nearbyPoints p n = union (adjacentPoints p) (concatMap adjacentPoints (nearbyPoints p (n-1)))

printPoints :: Space -> IO ()
printPoints s = mapM_ (putStrLn . show) s

numSpacesInWorld :: World -> Int
numSpacesInWorld World{ entities = ents } = length $ nub [[loc e] | e <- ents]

numEntitiesInWorld :: World -> Int
numEntitiesInWorld World{ entities = ents } = length ents

moveUp :: Entity -> Entity
moveUp Entity { loc = Point x y, .. } = Entity { loc = Point x (y+1), ..}

moveDown :: Entity -> Entity
moveDown Entity { loc = Point x y, .. } = Entity { loc = Point x (y-1), ..}

moveRight :: Entity -> Entity
moveRight Entity { loc = Point x y, .. } = Entity { loc = Point (x+1) y, ..}

moveLeft :: Entity -> Entity
moveLeft Entity { loc = Point x y, .. } = Entity { loc = Point (x-1) y, ..}

moveUpRight :: Entity -> Entity
moveUpRight Entity { loc = Point x y, .. } = Entity { loc = Point (x+1) (y+1), ..}

moveUpLeft :: Entity -> Entity
moveUpLeft Entity { loc = Point x y, .. } = Entity { loc = Point (x-1) (y+1), ..}

moveDownRight :: Entity -> Entity
moveDownRight Entity { loc = Point x y, .. } = Entity { loc = Point (x+1) (y-1), ..}

moveDownLeft :: Entity -> Entity
moveDownLeft Entity { loc = Point x y, .. } = Entity { loc = Point (x-1) (y-1), ..}

teleport :: Entity -> Loc -> Entity
teleport ent newLoc = ent { loc = newLoc }

someFunc :: IO ()
someFunc = do
    let player = Entity {
        symbol = 'P', 
        name = "Player", 
        loc = Point 0 0 , 
        control = adjacentPoints (Point 0 0)
        }
    let world = spawn genesis player
    putStrLn (show (numSpacesInWorld world))
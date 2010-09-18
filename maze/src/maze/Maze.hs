module Maze
  (
   Maze (..)
  ,Walls
  ,Location
  ,mkMaze
  ) where

import Data.Array
import Data.List
import System.Random ( StdGen, randomR )

type Walls = (Bool, Bool, Bool, Bool)
type Location = (Int,Int)
type Update = (Location,Walls)
newtype Maze = Maze (Array Location Walls) deriving (Show, Eq, Ord)

chooseLocation :: [(Location,Location)] -> StdGen -> ((Location,Location), StdGen)
chooseLocation cs g =
  let
    n = length cs
    (i,ng) = randomR (0,n-1) g
  in 
    (cs!!i,ng)

mergeWalls :: Update -> Update -> Update
mergeWalls (loc,(l,a,r,b)) (loc1,(l1,a1,r1,b1)) = (loc,(l&&l1,a&&a1,r&&r1,b&&b1))

mergeUpdates :: [Update] -> [Update]
mergeUpdates us =
  let
    firstEq (l,_) (r,_) = l == r
    firstOrd (l,_) (r,_) = compare l r
    ss = sortBy firstOrd us
    gs = groupBy firstEq ss
  in
    map (foldr mergeWalls ((1,1),(True, True, True, True))) gs

removeWall :: Walls -> Walls -> Location -> Location -> [Update]
removeWall (l,a,r,b) (l1,a1,r1,b1) loc1@(i,j) loc2@(n,m)
  | j == m+1 = [(loc1, (False,a,r,b)), (loc2, (l1,a1,False,b1))]
  | j == m-1 = [(loc1, (l,a,False,b)), (loc2, (False,a1,r1,b1))]
  | i == n+1 = [(loc1, (l,a,r,False)), (loc2, (l1,False,r1,b1))]
  | i == n-1 = [(loc1, (l,False,r,b)), (loc2, (l1,a1,r1,False))]

filterInRange :: Location -> Int -> Int -> [Location]
filterInRange (i,j) n m =
  let
    ls = [(i-1,j),(i+1,j),(i,j-1),(i,j+1)]
  in
    filter (inRange ((1,1),(n,m))) ls

getNeighbors :: Location -> [Location] -> Int -> Int -> [(Location,Location)]
getNeighbors (i,j) mazeElements n m =
  let
    fs = filterInRange (i,j) n m
    xs = filter (\i -> not $ elem i mazeElements) fs
  in
    map (\nl -> ((i,j),nl)) xs

mkPaths :: Maze -> [(Location,Location)] -> [Location] -> Int -> Int -> StdGen -> ([Update],StdGen)
mkPaths _ [] _ _ _ g = ([],g)
mkPaths maze@(Maze arr) candidates mazeElements n m g =
  let
    ((l,k),g2) = chooseLocation candidates g
    ws = removeWall (arr!l) (arr!k) l k
    ln = getNeighbors k mazeElements n m
    nme = (k:mazeElements)
    nc = filter (\(j,i) -> not $ elem i nme) candidates
    (ps,g3) = mkPaths maze (ln++nc) nme n m g2
  in
    (ws++ps,g3)

hasWall :: Maze -> Location -> Location -> Bool
hasWall (Maze arr) (i,j) (n,m)
    | j == m+1 = l
    | j == m-1 = r
    | i == n+1 = b
    | i == n-1 = a
    | otherwise = False
  where (l,a,r,b) = arr!(i,j)

removeRandomWalls :: Maze -> Int -> Int -> Int -> StdGen -> ([Update],StdGen)
removeRandomWalls _ _ _ 1 g = ([],g)
removeRandomWalls _ _ _ 0 g = ([],g)
removeRandomWalls maze@(Maze a) n m r g =
  let
    (x1,g1) = randomR (1,n) g
    (x2,g2) = randomR (1,m) g1
    x = (x1,x2)
    rxs1 = filterInRange x n m
    rxs = filter (hasWall maze x) rxs1
  in
    if ((length rxs) == 0) then
      removeRandomWalls maze n m (r-1) g2
    else
      let
        (w,g3) = randomR (0,(length rxs)-1) g2
        y = rxs!!w 
        rem = removeWall (a!x) (a!y) x y
        (cand, g4) = removeRandomWalls maze n m (r-2) g3
      in
        (rem++(cand),g4)

mkMaze :: Int -> Int -> Location -> StdGen -> (Maze, StdGen)
mkMaze n m s g =
  let
    arr = array ((1,1),(n,m)) [ ((i,j),(True,True,True,True)) | i <- [1..n], j <- [1..m] ]
    maze = Maze arr
    nbs = getNeighbors s [s] n m
    (ps,g2) = mkPaths maze nbs [s] n m g
    rn = 2 + ((n*m) `div` 100)
    arr2 =  arr // (mergeUpdates ps)
    (rps,g3) = removeRandomWalls (Maze arr2) n m (2*rn) g2
  in
    (Maze $ arr2 // (mergeUpdates rps),g3)

module Game
  (
   Game (..)
  ,GameState
  ,Heading (..)
  ,Path
  ,mkGame
  ,printGame
  ) where

import System.IO
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.Array
import Data.Maybe
import System.Random ( randomR, StdGen )
import Maze

data Heading = N | W | S | E deriving (Show, Eq, Ord)
type Path = [Location]
type GameState = StateT Game IO ()
newtype Game = Game (Maze, Heading, Path, Location, StdGen, Maybe Game)

instance Show Game where
  show (Game (Maze m, h, (p:_), _, _, _)) = getOptions $ rotateWalls h $ m!p

rotateWalls :: Heading -> Walls -> Walls
rotateWalls N w = w
rotateWalls W (l,a,r,b) = (b,l,a,r)
rotateWalls S (l,a,r,b) = (r,b,l,a)
rotateWalls E (l,a,r,b) = (a,r,b,l)

wallOrWay :: Bool -> String
wallOrWay b | b == True = "wall" | b == False = "corridor"

getOptions :: Walls -> String
getOptions (l,a,r,_) = "There is a "++(wallOrWay l)++" to the left, a "++
  (wallOrWay a)++" ahead, and a "++(wallOrWay r)++" to the right."
  
mkGame :: Int -> Int -> StdGen -> Game
mkGame n m g =
  let
    (x1,g1) = randomR (1,n) g
    (x2,g2) = randomR (1,m) g1
    (x3,g3) = randomR (1,n) g2
    (x4,g4) = randomR (1,m) g3
    start = (x1,x2)
    end = (x3,x4)
    (maze,g5) = mkMaze n m start g4
  in
    Game (maze, N, [start], end, g5, Nothing)

printHorizontalWall :: Bool -> Walls -> String
printHorizontalWall True  (_,True,_,_)   = " -" 
printHorizontalWall True  (_,False,_,_)  = "  "
printHorizontalWall False (_,_,_,True)   = " -"
printHorizontalWall False (_,_,_,False)  = "  "

printVerticalWall :: Location -> GameState
printVerticalWall ix = do
  Game (Maze m, h, p, e, _, _) <- get
  let
    (a,_,_,_) = m!ix
    start     = last p == ix
    exit      = e == ix
    path      = ix `elem` p
    x = case  (start,exit,path) of
              (True,_,_) -> 's'
              (_,True,_) -> 'e'
              (_,_,True) -> '*'
              _          -> ' '
  case a of
    True  -> lift $ putStr ['|',x]
    False -> lift $ putStr [' ',x]

printHorizontal :: Int -> Int -> Bool -> GameState
printHorizontal m i u = do
  Game (Maze a, _, _, _, _, _) <- get
  forM_ [ (a!(i,j)) | j <- [1..m] ] $ lift . putStr . (printHorizontalWall u)
  lift $ putStrLn " "

printVertical :: Int -> Int -> GameState
printVertical m i = do
  forM_ [ (i,j) | j <- [1..m] ] printVerticalWall
  lift $ putStrLn "|"

printSection :: Int -> Int -> GameState
printSection m i = do
  printVertical m i
  printHorizontal m i False

printGame :: GameState
printGame = do
  Game (Maze a, _, _, _, _, _) <- get
  let ((_,_),(n,m)) = bounds a
  printHorizontal m n True
  forM_ [n,(n-1)..1] $ printSection m
  lift $ hFlush stdout

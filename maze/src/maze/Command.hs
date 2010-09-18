module Command
  (
   Direction (..)
  ,Command (..)
  ,next
  ,checkSuccess
  ,rotateHeading
  ) where

import System.IO
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.Array
import Data.List
import Data.Maybe
import System.Random ( randomR, mkStdGen, StdGen )
import Maze
import Game

data Direction = LEFT | RIGHT deriving (Show, Eq, Ord, Read)
data Command = MOVE | TURN Direction | LOOK | NEW Int Int
             | SHOW | RESET | SAVE | LOAD deriving (Show, Eq, Ord, Read)

rotateHeading :: Heading -> Direction -> Heading
rotateHeading h LEFT = case h of { N -> W; W -> S; S -> E; E -> N }
rotateHeading h RIGHT = case h of { N -> E; W -> N; S -> W; E -> S }

increaseLocation :: Array Location Walls -> Heading -> Location -> Location
increaseLocation m h p@(i,j) =
    let (l,a,r,b) = m!p in
      case h of
        N -> if a then p else (i+1,j)
        W -> if l then p else (i,j-1)
        S -> if b then p else (i-1,j)
        E -> if r then p else (i,j+1)

next :: Command -> GameState
next MOVE = do
  Game (Maze m, h, p@(p1:ps), l, g, sp) <- get
  let i = increaseLocation m h p1
  put $ Game (Maze m, h, i:p, l, g, sp)

next (TURN d) = do
  Game (m, h, p, l, g, sp) <- get
  put $ Game (m, rotateHeading h d, p, l, g, sp)

next LOOK = return ()

next (NEW n m) = do
  Game (_, _, _, _, g, _) <- get
  put $ mkGame n m g

next SHOW = printGame

next RESET = do
  Game (m, _, p, l, g, sp) <- get
  put $ Game (m, N, [last p], l, g, Nothing)

next SAVE = do
  game@(Game (m, h, p, l, g, sp)) <- get
  put $ Game (m, h, p, l, g, Just game)

next LOAD = do
  Game (_, _, _, _, _, sp) <- get
  case sp of
    Nothing -> return ()
    Just g  -> put g

checkSuccess :: GameState
checkSuccess = do
  Game (Maze a, _, p:ps, l, gen, sp) <- get
  if p == l
    then
      do
        lift $ print "Congratulations. You did it."
        next SHOW
    else
      return ()

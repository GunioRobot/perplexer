module Main where

import System
import System.IO
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import System.Random ( mkStdGen )
import Maze
import Game
import Command

loop = forever (do
  checkSuccess

  -- print state
  g <- get
  lift $ print g
  lift $ hFlush stdout

  -- get next command and execute
  l <- lift getLine
  c <- return $ reads l
  case c of
    [] -> return ()
    (c1,_):cs -> next c1)
  
main = do
  n:m:seed:[] <- (liftM $ map read) getArgs
  s <- execStateT loop $ mkGame n m $ mkStdGen seed
  return ()

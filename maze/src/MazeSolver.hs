module Main where

import Data.List
import System
import System.IO
import Control.Monad
import Data.Either
import Control.Concurrent

data Status = Status (Bool,Bool,Bool) | SUCCESS deriving (Show, Eq)
data Direction = LEFT | RIGHT deriving (Show, Eq, Read)
data Command = MOVE | BACK | TURN Direction | SHOW | RESET deriving (Show, Eq, Read)
data Heading = N | W | S | E deriving (Show, Eq)
type Location = (Int, Int)
type Path = ([Command],[Location])
type MazeMove = FilePath -> FilePath -> Status -> Heading -> Location -> [Location] -> IO (Either Path Path)

move :: MazeMove
move _ _ SUCCESS _ _ l = return $ Right ([],l)
move fIn fOut status h cur@(i,j) ls
  | (i,j) `elem` ls = return $ Left ([],ls)
  | otherwise = do
      s <- tryTurn LEFT fIn fOut status h cur ls
      s <- either (\(c,l) -> tryAhead fIn fOut status h cur l) (return . Right) s
      either (\(c,l) -> tryTurn RIGHT fIn fOut status h cur l) (return . Right) s

tryTurn :: Direction -> MazeMove
tryTurn LEFT  _ _ (Status (True,_,_)) _ _ l = return $ Left ([],l)
tryTurn RIGHT _ _ (Status (_,_,True)) _ _ l = return $ Left ([],l)
tryTurn dir fIn fOut _ h cur l = do
  let rdir = case dir of { LEFT -> RIGHT; RIGHT -> LEFT }
  s <- execute fIn fOut $ TURN dir
  s' <- tryAhead fIn fOut s (rotateHeading h dir) cur l
  case s' of
    Right (c,l') -> return $ Right ((TURN dir):c,l')
    l' -> do { execute fIn fOut $ TURN rdir; return l' }

tryAhead :: MazeMove
tryAhead _ _ (Status (_,True,_)) _ _ l = return $ Left ([],l)
tryAhead fIn fOut _ h cur ls = do
  let incr h (i,j) = case h of { N -> (i,j+1); S -> (i,j-1); W -> (i-1,j); E -> (i+1,j) }
  s <- execute fIn fOut MOVE
  s' <- move fIn fOut s h (incr h cur) $ cur:ls
  putStr "." >> hFlush stdout
  case s' of
    Right (c,l') -> return $ Right (MOVE:c,l')
    l' -> do { execute fIn fOut BACK; return l' }

instance Read Status where
  readsPrec _ s = case l1 of
        "\"Congratulations." -> [(SUCCESS,"")]
        _ -> [(Status (wallOrWay (l!!3), wallOrWay (l!!8), wallOrWay (l!!12)),"")]
      where l@(l1:ls) = words s
            wallOrWay s = case s of { "wall" -> True; "corridor" -> False }

executeRaw :: FilePath -> Command -> IO ()
executeRaw fOut cmd = withFile fOut WriteMode $ \h -> hPrint h cmd

execute :: FilePath -> FilePath -> Command -> IO Status
execute fIn fOut command = do
  executeRaw fOut command
  withFile fIn ReadMode $ liftM read . hGetLine

consume :: FilePath -> Bool -> IO ()
consume fIn silent = do
  hIn <- openFile fIn ReadMode
  hSetBuffering hIn NoBuffering
  l <- hGetLine hIn
  if (isPrefixOf "There" l || isPrefixOf "\"Con" l || silent)
    then threadDelay 20000 else  putStrLn l
  b <- hReady hIn
  hClose hIn
  if b then consume fIn silent else return ()

rotateHeading h LEFT  = case h of { N -> W; W -> S; S -> E; E -> N }
rotateHeading h RIGHT = case h of { N -> E; W -> N; S -> W; E -> S }

main = do
  fIn:fOut:[] <- getArgs
  executeRaw fOut RESET >> consume fIn True
  putStrLn "\nHere is the Maze:" >> executeRaw fOut SHOW >> consume fIn False
  putStrLn "\nSolving Maze. Hang tight."
  t <- execute fIn fOut RESET
  s <- move fIn fOut t N (0,0) []
  (c,l) <- case s of
    Right (c,l) -> return (c,l)
    Left (c,l) -> do
      t <- execute fIn fOut $ TURN LEFT
      Right (c,l) <- move fIn fOut t W (0,0) (filter (/=(0,0)) l)
      return ((TURN LEFT):c,l)
  putStrLn "\n\nHere's the path:" >> print c
  executeRaw fOut RESET >> consume fIn True
  putStr "\nWalking the path..." >> (forM_ c $ executeRaw fOut)
  putStrLn "\nHere's the solved maze:\n" >> hFlush stdout >> consume fIn False

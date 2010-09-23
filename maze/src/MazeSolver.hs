module Main where

import Data.List
import System
import System.IO
import Control.Monad
import System.Random ( mkStdGen )
import Control.Concurrent

data Status = Status (Bool,Bool,Bool) | SUCCESS deriving (Show, Eq)
data Direction = LEFT | RIGHT deriving (Show, Eq, Ord, Read)
data Command = MOVE | TURN Direction | SHOW | RESET deriving (Show, Eq, Ord, Read)
data Heading = N | W | S | E deriving (Show, Eq, Ord)             

rotateHeading :: Heading -> Direction -> Heading
rotateHeading h LEFT = case h of { N -> W; W -> S; S -> E; E -> N }
rotateHeading h RIGHT = case h of { N -> E; W -> N; S -> W; E -> S }

wallOrWay :: String -> Bool
wallOrWay "wall" = True
wallOrWay "corridor" = False

instance Read Status where
  readsPrec i s =
    let l@(l1:ls) = words s in
      case l1 of
        "\"Congratulations." -> [(SUCCESS,"")]
        _ -> [(Status (wallOrWay (l!!3), wallOrWay (l!!8), wallOrWay (l!!12)),"")]

executeRaw :: FilePath -> Command -> IO ()
executeRaw fOut cmd = withFile fOut AppendMode (\h -> hPrint h cmd)
        
executeRaw' :: FilePath -> [Command] -> IO ()
executeRaw' fOut cs = forM_ cs $ executeRaw fOut

execute :: FilePath -> FilePath -> Command -> IO Status
execute fIn fOut command = do
  executeRaw fOut command
  withFile fIn ReadMode $ liftM read . hGetLine

execute' :: FilePath -> FilePath -> [Command] -> IO ()
execute' fIn fOut cs = forM_ cs $ execute fIn fOut

incr :: Heading -> (Int,Int) -> (Int,Int)
incr h (i,j) = case h of { N -> (i+1,j); S -> (i-1,j); W -> (i,j+1); E -> (i,j-1) }

tryAhead :: FilePath -> FilePath -> Status -> Heading -> [(Int,Int)] -> IO (Status,[Command],[(Int,Int)])
tryAhead _ _ s@(Status (_,True,_)) _ l = return (s,[],l)
tryAhead fIn fOut _ h l@(l1:ls) = do
  s <- execute fIn fOut MOVE
  (s',c',l') <- move s fIn fOut h $ (incr h l1):l
  case s' of
    SUCCESS -> return ()
    _ -> execute' fIn fOut [TURN LEFT,TURN LEFT,MOVE,TURN LEFT,TURN LEFT]
  putStr "."
  hFlush stdout
  return (s',MOVE:c',l')

tryTurn :: Direction -> FilePath -> FilePath -> Status -> Heading -> [(Int,Int)] -> IO (Status,[Command],[(Int,Int)])
tryTurn LEFT _ _ s@(Status (True,_,_)) _ l = return (s,[],l)
tryTurn RIGHT _ _ s@(Status (_,_,True)) _ l = return (s,[],l)
tryTurn dir fIn fOut status h l = do
  let rdir = case dir of { LEFT -> RIGHT; RIGHT -> LEFT }
  s <- execute fIn fOut $ TURN dir
  (s',c',l') <- tryAhead fIn fOut s (rotateHeading h dir) l
  case s' of
    st@SUCCESS -> return st
    _ -> execute fIn fOut $ TURN rdir
  return (s',(TURN dir):c',l')

move :: Status -> FilePath -> FilePath -> Heading -> [(Int,Int)] -> IO (Status,[Command],[(Int,Int)])
move s@SUCCESS _ _ _ ls = return (s,[],ls)
move status@(Status (b1,b2,b3)) fIn fOut h el@((i,j):ls)
  | (i,j) `elem` ls = return (status,[],ls)
  | otherwise = do
      (s,c,l) <- tryTurn LEFT fIn fOut status h el
      case s of
        SUCCESS -> return (s,c,l)
        _ -> do
          (s,c,l) <- tryAhead fIn fOut status h el
          case s of
            SUCCESS -> return (s,c,l)
            _ -> do
              (s,c,l) <- tryTurn RIGHT fIn fOut status h el
              return (s,c,l)
  
consume :: FilePath -> Bool -> IO ()
consume fIn silent = do
  hIn <- openFile fIn ReadMode
  hSetBuffering hIn NoBuffering
  l <- hGetLine hIn
  if (isPrefixOf "There" l || isPrefixOf "\"Congrat" l || silent)
    then threadDelay 20000
    else putStrLn l
  b <- hReady hIn
  hClose hIn
  if b then consume fIn silent else return ()

main :: IO ()
main = do
  (fIn:fOut:[]) <- getArgs
  executeRaw fOut RESET
  consume fIn True
  executeRaw fOut SHOW
  putStrLn "\nHere is the Maze"
  consume fIn False
  putStrLn "\nSolving Maze. Hang tight."
  t <- execute fIn fOut RESET
  (s,c,l) <- move t fIn fOut N [(0,0)]
  (s,c,l) <- case s of
    SUCCESS -> return (s,c,l)
    otherwise -> do
      t <- execute fIn fOut $ TURN LEFT
      (s,c,l) <- move t fIn fOut W l
      return (s,(TURN LEFT):c,l)
  putStrLn "\n\nHere's the path:"
  print c
  putStr "\nWalking the path..."
  executeRaw fOut RESET
  consume fIn True
  executeRaw' fOut c
  putStrLn "\nHere's the solved maze:\n"
  consume fIn False

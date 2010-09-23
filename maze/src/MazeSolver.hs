module Main where

import Data.List
import System
import System.IO
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import System.Random ( mkStdGen )
import Debug.Trace
import Control.Concurrent

data Status = Status (Bool,Bool,Bool) | SUCCESS deriving (Show, Eq)
data Direction = LEFT | RIGHT deriving (Show, Eq, Ord, Read)
data Command = MOVE | TURN Direction | LOOK | NEW Int Int
             | SHOW | RESET | SAVE | LOAD deriving (Show, Eq, Ord, Read)
data Heading = N | W | S | E deriving (Show, Eq, Ord)             

rotateHeading :: Heading -> Direction -> Heading
rotateHeading h LEFT = case h of { N -> W; W -> S; S -> E; E -> N }
rotateHeading h RIGHT = case h of { N -> E; W -> N; S -> W; E -> S }

wallOrWay :: String -> Bool
wallOrWay "wall" = True
wallOrWay "corridor" = False

instance Read Status where
  readsPrec i s = do
    (s1, r1) <- lex s
    case s1 of
      "\"Congratulations. You did it.\"" -> return (SUCCESS,r1)
      _         -> do
        (_, r1)  <- lex r1
        (_, r1)  <- lex r1
        (s1, r1) <- lex r1
        b1 <- return $ wallOrWay s1
        (_, r1)  <- lex r1
        (_, r1)  <- lex r1
        (_, r1)  <- lex r1
        (_, r1)  <- lex r1
        (_, r1)  <- lex r1
        (s1, r1) <- lex r1
        b2 <- return $ wallOrWay s1
        (_, r1)  <- lex r1
        (_, r1)  <- lex r1
        (_, r1)  <- lex r1
        (_, r1)  <- lex r1
        (s1, r1)  <- lex r1
        b3 <- return $ wallOrWay s1
        return (Status (b1,b2,b3),"")
              
execute' :: FilePath -> FilePath -> [Command] -> IO ()
execute' fIn fOut cs = do
  forM_ cs (execute fIn fOut)

executeRaw' :: FilePath -> [Command] -> IO ()
executeRaw' fOut cs = do
  forM_ cs (executeRaw fOut)

executeRaw :: FilePath -> Command -> IO ()
executeRaw fOut cmd = withFile fOut AppendMode (\h -> hPrint h cmd)

execute :: FilePath -> FilePath -> Command -> IO Status
execute fIn fOut command = do
  executeRaw fOut command
  withFile fIn ReadMode getStatus

getStatus :: Handle -> IO Status
getStatus hIn = do
  line <- hGetLine hIn
  return $ read line

incr :: Heading -> (Int,Int) -> (Int,Int)
incr N (i,j) = (i+1,j)
incr S (i,j) = (i-1,j)
incr W (i,j) = (i,j+1)
incr E (i,j) = (i,j-1)

tryAhead :: FilePath -> FilePath -> Status -> Heading -> [(Int,Int)] -> IO (Status,[Command],[(Int,Int)])
tryAhead _ _ s@(Status (_,True,_)) _ l = return (s,[],l)
tryAhead fIn fOut _ h l@(l1:ls) = do
  s <- execute fIn fOut MOVE
  (s2,c,l2) <- move s fIn fOut h ((incr h l1):l)
  case s2 of
    SUCCESS -> return ()
    _ -> execute' fIn fOut [TURN LEFT,TURN LEFT,MOVE,TURN LEFT,TURN LEFT]
  putStr "."
  hFlush stdout
  return (s2,MOVE:c,l2)

tryTurn :: Direction -> FilePath -> FilePath -> Status -> Heading -> [(Int,Int)] -> IO (Status,[Command],[(Int,Int)])
tryTurn LEFT _ _ s@(Status (True,_,_)) _ l = return (s,[],l)
tryTurn RIGHT _ _ s@(Status (_,_,True)) _ l = return (s,[],l)
tryTurn dir fIn fOut status h l = do
  let
    rdir = case dir of
      LEFT -> RIGHT
      RIGHT -> LEFT
  s <- execute fIn fOut $ TURN dir
  res@(s2,c2,l2) <- tryAhead fIn fOut s (rotateHeading h dir) l
  case s2 of
    SUCCESS -> return ()
    _ -> do { execute fIn fOut $ TURN rdir; return () }
  return (s2,(TURN dir):c2,l2)

move :: Status -> FilePath -> FilePath -> Heading -> [(Int,Int)] -> IO (Status, [Command], [(Int,Int)])
move s@SUCCESS _ _ _ ls = do { putStrLn ""; return (s, [], ls) }
move status@(Status (b1,b2,b3)) fIn fOut h el@((i,j):ls)
  | (i,j) `elem` ls = return (status, [], ls)
  | otherwise = do
      (s, c, l) <- tryTurn LEFT fIn fOut status h el
      case s of
        SUCCESS -> return (s,c,l)
        _ -> do
          (s, c, l) <- tryAhead fIn fOut status h el
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
  if ((isPrefixOf "There" l) || (isPrefixOf "\"Congrat" l) || silent)
     then
       threadDelay 20000
     else
       trace l return ()
  b <- hReady hIn
  hClose hIn
  if (b)
    then
      do
        consume fIn silent
    else
      return ()

main :: IO ()
main = do
  (fIn:fOut:[]) <- getArgs
  executeRaw fOut RESET
  consume fIn True
  executeRaw fOut SHOW
  putStrLn ""
  putStrLn "Here is the Maze"
  consume fIn False
  putStrLn ""
  putStrLn "Solving Maze. Hang tight."
  t <- execute fIn fOut RESET
  (s,c,l) <- move t fIn fOut N [(0,0)]
  (s,c,l) <- case s of
    SUCCESS -> return (s,c,l)
    otherwise -> do
      t <- execute fIn fOut $ TURN LEFT
      (s,c,l) <- move t fIn fOut W l
      return (s,(TURN LEFT):c,l)
  putStrLn ""
  putStrLn ""
  putStrLn "Here's the path:"
  print c
  putStrLn ""
  putStr "Walking the path..."
  executeRaw fOut RESET
  consume fIn True
  executeRaw' fOut c
  putStrLn ""
  putStrLn "Here's the solved maze:"
  putStrLn ""
  consume fIn False

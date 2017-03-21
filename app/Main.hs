{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import System.Console.ANSI
import System.IO
import System.Posix.Unistd
import System.Random
import Control.Concurrent
import Control.Monad
import Control.Monad.Loops
import Control.Monad.IO.Class
import Data.List
import Data.IORef
import Data.Maybe
import GHC.Generics

delay = 500000 -- in microseconds
windowX = 15
windowY = 15

data GridState = GridState { gsX :: Int, gsY :: Int, alive :: Bool } deriving (Show, Generic)

-- Game rules --
getGridAlive :: Int -> Int -> [GridState] -> Bool
getGridAlive _ _ []     = False
getGridAlive x y g = if (length r) > 0 then (alive . head) r else False
    where r = foldMap (\i -> if gsX i == x && gsY i == y then [i] else []) g

checkGridAlive :: [GridState] -> GridState -> GridState
checkGridAlive gs g
  | ga && (ln < 2 || ln > 3) = GridState gx gy False
  | not ga && (ln == 3)      = GridState gx gy True
  | otherwise                = GridState gx gy ga
    where gx = gsX g
          gy = gsY g
          ga = alive g
          tt = [ getGridAlive (gx + x) (gy + y) gs | x <- [-1..1], y <- [-1..1] ] -- truth table
          -- Alive neighbors
          ln = (sum $ map fromEnum tt) - fromEnum ga

gol :: [GridState] -> [GridState]
gol g = map (checkGridAlive g) g

-- Displaying GridState on ASCII --
isNewLine :: GridState -> String
isNewLine g
  | nl && end = "|"
  | nl = "|\n"
  | otherwise = ""
  where nl  = (==) (windowX-1) (gsX g)
        end = (==) (windowX-1) (gsX g) && (==) (windowY-1) (gsY g)

randomBool :: StdGen -> [Bool]
randomBool s = take (windowX * windowY) $ unfoldr (Just . random) s :: [Bool]

genGridState :: [Bool] -> [GridState]
genGridState bB = [ GridState cx cy cb | (GridState cx cy _, cb) <- zip ig bB ]
  where ig = [ GridState (x-1) (y-1) False | y <- [1..windowY], x <- [1..windowX] ]

strGridState :: GridState -> String
strGridState gs
  | x         = "x" ++ nl
  | otherwise = " " ++ nl
  where x = alive gs
        nl = isNewLine gs

putStrGridState :: [GridState] -> IO ()
putStrGridState g = do
    clearScreen
    putStrLn $ intercalate "" [ "=" | _ <- [0..windowX*2] ]
    putStrLn $ intercalate "" [ "|" ++ strGridState cg | cg <- g ]
    putStrLn $ intercalate "" [ "=" | _ <- [0..windowX*2] ]
    usleep delay


main :: IO ()
main = do
    s <- newStdGen
    let grids = genGridState (randomBool s)
    iterateM_ (\w -> putStrGridState w >> return (gol w)) grids


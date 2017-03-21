{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import System.Random
import System.Console.ANSI
import System.IO
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.IORef
import Data.Maybe
import GHC.Generics

windowX = 10
windowY = 10

data GridState = GridState { gsX :: Int, gsY :: Int, alive :: Bool } deriving (Show, Generic)

isNewLine :: GridState -> String
isNewLine g
    | nl && end = "|"
    | nl = "|\n"
    | otherwise = ""
    where nl  = (==) windowX (gsX g)
          end = (==) windowX (gsX g) && (==) windowY (gsY g)

randomBool :: StdGen -> [Bool]
randomBool s = take (windowX * windowY) $ unfoldr (Just . random) s :: [Bool]

genGridState :: [Bool] -> [GridState]
genGridState bB = [ GridState cx cy cb | (GridState cx cy _, cb) <- zip ig bB ]
    where ig = [ GridState x y False | y <- [1..windowY], x <- [1..windowX] ]

strGridState :: GridState -> String
strGridState gs
    | x         = "x" ++ nl
    | otherwise = " " ++ nl
    where x = alive gs
          nl = isNewLine gs

putStrGridState :: [GridState] -> IO ()
putStrGridState g = do
    putStrLn $ intercalate "" [ "=" | _ <- [0..windowX*2] ]
    putStrLn $ intercalate "" [ "|" ++ strGridState cg | cg <- g ]
    putStrLn $ intercalate "" [ "=" | _ <- [0..windowX*2] ]

main :: IO ()
main = do
    s <- newStdGen
    let grids = genGridState (randomBool s)
    putStrGridState grids


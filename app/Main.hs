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

randomBool :: StdGen -> Int -> [Bool]
randomBool s n = take n $ unfoldr (Just . random) s :: [Bool]

genGridState :: Int -> Int -> [Bool] -> [GridState]
genGridState maxX maxY bB = [ GridState cx cy cb | (GridState cx cy _, cb) <- zip ig bB ]
    where ig = [ GridState x y False | x <- [0..maxX], y <- [0..maxY] ]

strGridState :: GridState -> String
strGridState gs
    | x         = "x"
    | otherwise = " "
    where x = alive gs

main :: IO ()
main = do
    s <- newStdGen
    let grids = genGridState windowX windowY (randomBool s (windowX * windowY))
    print grids


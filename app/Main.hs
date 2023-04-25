module Main (main) where

{-# OPTIONS_GHC -package mtl #-}


import           BrainFuck2

main :: IO ()
main = do
    input <-  lines <$> getContents
    runBrainFuck $ concat input

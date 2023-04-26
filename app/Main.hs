module Main (main) where

{-# OPTIONS_GHC -package mtl #-}


import           BrainFuck2

main :: IO ()
main = do
    input <- readFile "resource/halloWorld.bf"


    -- input <-  lines <$> getContents
    runBrainFuck input -- $ concat input

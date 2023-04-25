module Main (main) where

{-# OPTIONS_GHC -package mtl #-}


import           BrainFuck

main :: IO ()
main = do
    input <-  lines <$> getContents
    interpretBF $ concat input

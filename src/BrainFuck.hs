module BrainFuck (interpretBF) where

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -package mtl #-}

import           Control.Monad.State
import           Data.Char
import           Prelude             hiding (putChar)

data Operator = Increase | Decrease | MoveRight | MoveLeft | Output | Input | Loop [Operator]
  deriving (Show, Eq)


newtype Tape a = Tape ([a], a, [a])


parseBrainFuckOp :: [Char] -> [Operator]
parseBrainFuckOp ('>':xs) = MoveRight : parseBrainFuckOp xs
parseBrainFuckOp ('<':xs) = MoveLeft  : parseBrainFuckOp xs
parseBrainFuckOp ('+':xs) = Increase :  parseBrainFuckOp xs
parseBrainFuckOp ('-':xs) = Decrease :  parseBrainFuckOp xs
parseBrainFuckOp ('.':xs) = Output :   parseBrainFuckOp xs
parseBrainFuckOp (',':xs) = Input :   parseBrainFuckOp xs
parseBrainFuckOp ('[':xs) = Loop (parseBrainFuckOp xsB) : parseBrainFuckOp xsA
    where   xsB = takeTillMatchingBracket xs 1
            xsA = drop (length xsB) xs
parseBrainFuckOp (']':xs) = parseBrainFuckOp xs
parseBrainFuckOp ('@':_) = []
parseBrainFuckOp (_:xs) = parseBrainFuckOp xs
parseBrainFuckOp [] = []


takeTillMatchingBracket :: [Char] -> Int -> [Char]
takeTillMatchingBracket (']':_) 1   =  []
takeTillMatchingBracket (']':xs) count = ']': takeTillMatchingBracket xs (count - 1)
takeTillMatchingBracket ('[':xs) count = '[': takeTillMatchingBracket xs (count + 1)
takeTillMatchingBracket (x:xs) count = x : takeTillMatchingBracket xs count
takeTillMatchingBracket _ _          =  []


ogTape :: Tape Int
ogTape = Tape ([], 0, replicate 30000 0)

eval :: [Operator] -> State (Tape Int) String
eval (Increase:xs)   = processM increase xs
eval (Decrease:xs)   = processM decrease xs
eval (Output:xs)     = processM putChar xs
eval (MoveRight:xs)  = processM moveRight xs
eval (MoveLeft:xs)   = processM moveLeft xs
eval ((Loop ops):xs) = processM (loop ops) xs
eval _               = return ""

processM :: State (Tape Int) String -> [Operator] -> State (Tape Int) String
processM f xs = do
    y <- f
    ys <- eval xs
    return $ y ++ ys


increase :: State (Tape Int) String
increase = do
    Tape (sx, x, xs) <- get
    let resultTape = Tape (sx, mod (x + 1) 256, xs)
    put resultTape
    return ""


decrease :: State (Tape Int) String
decrease = do
    Tape (sx, x, xs) <- get
    let resultTape = Tape (sx, if x == 0 then 255 else x -1, xs)
    put resultTape
    return ""

putChar :: State (Tape Int) String
putChar = do
    Tape (_, x, _) <- get
    return [chr x]


moveRight :: State (Tape Int) String
moveRight = do
    Tape (sx, x, xs) <- get
    put $ Tape (x:sx , head xs, tail xs)
    return ""


moveLeft :: State (Tape Int) String
moveLeft = do
    Tape (sx, x, xs) <- get
    unless (null sx) $ do put $ Tape (tail sx, head sx, x: xs)
    return ""

loop :: [Operator] -> State (Tape Int) String
loop ys = do
    Tape (_, x, _) <- get
    if x == 0
    then return ""
    else do
        y <- eval ys
        zs <- loop ys
        return $ y ++ zs



interpretBF :: String -> IO ()
interpretBF input = do

    let ops = parseBrainFuckOp input

    let (str,_) = runState (eval ops) ogTape


    putStr str
    -- return str

{-
fromString :: String -> String
fromString xs = evalState (eval (parseBrainFuckOp  xs)) ogTape


helloworld = fromString "++++++++++[>+>+++>+++++++>++++++++++<<<<-]>>>++.>+.+++++++..+++.<<++.>+++++++++++++++.>.+++.------.--------.<<+.<."

char = fromString ".+[.+]"


test = "+[+++++++++++++++++++++++++++++++++++>]<."
-}

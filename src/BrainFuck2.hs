module BrainFuck2 (runBrainFuck) where

import           Data.Char
import           System.IO (hFlush, stdout)




data Operator = Increase | Decrease | MoveRight | MoveLeft | Output | Input | Loop [Operator]
  deriving (Show, Eq)

newtype Tape = Tape ([Int], Int, [Int])

emptyTape :: Tape
emptyTape = Tape ([], 0, replicate 30000 0)

eval :: Tape -> String -> IO Tape
eval tape (x:xs) = case x of
    '>' -> eval (moveRight tape) xs
    '<' -> eval (moveLeft tape) xs
    '+' -> eval (increase tape) xs
    '-' -> eval (decrease tape) xs
    '.' -> do
            putChar (chr (get tape))
            _ <- hFlush stdout
            eval tape xs
    ',' -> do
        c <- getChar
        eval (set tape (ord c)) xs
    '[' -> do
            afterLoopTape <- loopTape tape betweenBrackets
            eval afterLoopTape outsideBrackets
    _   -> eval tape xs
    where   betweenBrackets = takeTillBracket xs 1
            outsideBrackets = drop (length betweenBrackets) xs
eval tape [] = return tape


takeTillBracket :: [Char] -> Int -> [Char]
takeTillBracket (x:xs) count = case x of
    ']' | count == 1 -> []
        | otherwise -> ']' : takeTillBracket xs (count -1)
    '[' -> '[' : takeTillBracket xs (count +1)
    y -> y : takeTillBracket xs count
takeTillBracket [] _ = []

moveRight :: Tape -> Tape
moveRight (Tape (xsL,x,xsR)) = Tape (x:xsL,head xsR,tail xsR)


moveLeft :: Tape -> Tape
moveLeft (Tape (xsL,x,xsR)) = if not (null xsL)
                                then Tape (tail xsL, head xsL, x: xsR)
                                else Tape (xsL,x,xsR)

increase :: Tape -> Tape
increase (Tape (xsL,x,xsR)) = Tape (xsL,mod (x + 1) 256,xsR)


decrease :: Tape -> Tape
decrease (Tape (xsL,x,xsR)) = Tape (xsL, if x == 0 then 255 else x -1, xsR)


get :: Tape -> Int
get (Tape (_,x,_)) = x

set :: Tape -> Int -> Tape
set (Tape (l,_,r)) x = Tape (l,x,r)

loopTape :: Tape -> String -> IO Tape
loopTape tape xs = do
    if get tape /= 0
        then do
                afterLooptape <- eval tape xs
                loopTape afterLooptape xs
        else return tape


runBrainFuck :: String -> IO ()
runBrainFuck input = do
    _ <- eval emptyTape input
    return ()


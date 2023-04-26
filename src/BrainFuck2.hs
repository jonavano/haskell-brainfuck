module BrainFuck2 (runBrainFuck) where

import           Data.Char (chr, ord)
import           Data.Word (Word8)


-- newtype Tape = Tape ([Word8], Word8, [Word8])
data Tape = Tape ![Word8] !Word8 [Word8]


emptyTape :: Tape
emptyTape = Tape [] 0 (replicate 30000 0)

eval :: Tape -> String -> IO Tape
eval tape (x:xs) = case x of
    '+' -> eval (increase tape) xs
    '-' -> eval (decrease tape) xs
    '>' -> eval (moveRight tape) xs
    '<' -> eval (moveLeft tape) xs
    '[' -> parseBrackets tape xs
    '.' -> output tape xs
    ',' -> inputChar tape xs
    _   -> eval tape xs

eval tape [] = return tape


takeTillBracket :: [Char] -> Int -> [Char]
takeTillBracket (x:xs) count = case x of
    ']' | count == 1 -> []
        | otherwise -> ']' : takeTillBracket xs (count -1)
    '[' -> '[' : takeTillBracket xs (count +1)
    y -> y : takeTillBracket xs count
takeTillBracket [] _ = error "made it till end without bracket closure"

moveRight :: Tape -> Tape
moveRight (Tape xsL x xsR) = Tape (x:xsL) (head xsR) (tail xsR)


moveLeft :: Tape -> Tape
moveLeft (Tape xsL x xsR) = if not (null xsL)
                                then Tape (tail xsL) (head xsL) (x: xsR)
                                else Tape xsL x xsR

increase :: Tape -> Tape
increase (Tape xsL x xsR) = Tape xsL (x + 1) xsR-- Tape (xsL,mod (x + 1) 256,xsR)


decrease :: Tape -> Tape
decrease (Tape xsL x xsR) = Tape xsL  (x -1)  xsR -- Tape (xsL, if x == 0 then 255 else x -1, xsR)


get :: Tape -> Word8
get (Tape _ x _ ) = x

set :: Tape -> Word8 -> Tape
set (Tape l _ r ) x = Tape l x r

output :: Tape -> String -> IO Tape
output tape xs = do
            putChar (chr (fromIntegral (get tape)))
            eval tape xs

inputChar :: Tape -> String -> IO Tape
inputChar tape xs = do
        c <- getChar
        eval (set tape (fromIntegral (ord c))) xs

loopTape :: Tape -> String -> IO Tape
loopTape tape xs = do
    if get tape /= 0
        then do
                afterLooptape <- eval tape xs
                loopTape afterLooptape xs
        else return tape

parseBrackets :: Tape -> String -> IO Tape
parseBrackets tape xs = do
            afterLoopTape <- loopTape tape betweenBrackets
            eval afterLoopTape outsideBrackets
            where   betweenBrackets = takeTillBracket xs 1
                    outsideBrackets = drop (length betweenBrackets) xs

runBrainFuck :: String -> IO ()
runBrainFuck input = do
    _ <- eval emptyTape input
    return ()


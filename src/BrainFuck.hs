module BrainFuck where

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# OPTIONS_GHC -package mtl #-}s

-- {-# OPTIONS_GHC -fwarn-incomplete-patterns \#-}
-- import qualified Control.Monad.ST as State
-- import           Control.Monad.Trans
import           Control.Monad.State
import           Data.Char
import           Prelude             hiding (putChar)
-- import           System.Directory.Internal.Prelude (getArgs)

data Operator = Increase | Decrease | MoveRight | MoveLeft | PutChar | GetChar | Loop [Operator]
  deriving (Show, Eq)


newtype Tape a = Tape ([a], a, [a])


parseBrainFuckOp :: [Char] -> [Operator]
parseBrainFuckOp ('>':xs) = MoveRight : parseBrainFuckOp xs
parseBrainFuckOp ('<':xs) = MoveLeft  : parseBrainFuckOp xs
parseBrainFuckOp ('+':xs) = Increase :  parseBrainFuckOp xs
parseBrainFuckOp ('-':xs) = Decrease :  parseBrainFuckOp xs
parseBrainFuckOp ('.':xs) = PutChar :   parseBrainFuckOp xs
parseBrainFuckOp (',':xs) = GetChar :   parseBrainFuckOp xs
parseBrainFuckOp ('[':xs) = Loop (parseBrainFuckOp xsB) : parseBrainFuckOp xsA
    where   xsB = takeTillMatchingBracket xs 1
            xsA = drop (length xsB) xs
parseBrainFuckOp (']':xs) = parseBrainFuckOp xs
parseBrainFuckOp ('@':xs) = []
parseBrainFuckOp (x:xs) = parseBrainFuckOp xs
parseBrainFuckOp [] = []


takeTillMatchingBracket :: [Char] -> Int -> [Char]
takeTillMatchingBracket (']':xs) 1   =  []
takeTillMatchingBracket (']':xs) count = ']': takeTillMatchingBracket xs (count - 1)
takeTillMatchingBracket ('[':xs) count = '[': takeTillMatchingBracket xs (count + 1)
takeTillMatchingBracket (x:xs) count = x : takeTillMatchingBracket xs count
takeTillMatchingBracket _ _          =  []



ogTape = Tape ([], 0, replicate 30000 0)

eval :: [Operator] -> State (Tape Int) String
eval (Increase:xs)   = processM increase xs
eval (Decrease:xs)   = processM decrease xs
eval (PutChar:xs)    = processM putChar xs
eval (MoveRight:xs)  = processM moveRight xs
eval (MoveLeft:xs)   = processM moveLeft xs
eval ((Loop ops):xs) = processM (loop ops) xs
eval _               = return ""

processM f xs = do
    y <- f
    ys <- eval xs
    return $ y ++ ys

-- {-# INLINE increase #-}
increase :: State (Tape Int) String
increase = do
    Tape (sx, x, xs) <- get
    let resultTape = Tape (sx, mod (x + 1) 256, xs)
    put resultTape
    return ""

-- {-# INLINE decrease #-}
decrease :: State (Tape Int) String
decrease = do
    Tape (sx, x, xs) <- get
    let resultTape = Tape (sx, if x == 0 then 255 else x -1, xs)
    put resultTape
    return ""

-- {-# INLINE putChar #-}
putChar :: State (Tape Int) String
putChar = do
    Tape (_, x, _) <- get
    return [chr x]

-- {-# INLINE moveRight #-}
moveRight :: State (Tape Int) String
moveRight = do
    Tape (sx, x, xs) <- get
    put $ Tape (x:sx , head xs, tail xs)
    return ""

-- {-# INLINE moveLeft #-}
moveLeft :: State (Tape Int) String
moveLeft = do
    Tape (sx, x, xs) <- get
    if null sx
        then return ""
        else do
                put $ Tape (tail sx, head sx, x: xs)
                return ""

loop :: [Operator] -> State (Tape Int) String
loop ys = do
    Tape (sx, x, xs) <- get
    -- if mod x 256 == 0
    if x == 0
    then return ""
    else do
        y <- eval ys
        -- let (y,resultState) = runState (eval ys) $ Tape (sx, x, xs)
        -- put resultState
        zs <- loop ys
        return $ y ++ zs





bfmain :: IO ()
bfmain = do
    -- str <- getContents
    -- args <- getArgs
    input <-  lines <$> getContents
    -- putStr input

    let ops = parseBrainFuckOp $ concat input -- args

    let (str,resultState) = runState (eval ops) ogTape

    -- let opsStr = concatMap show ops
    -- putStrLn opsStr

    putStr str


fromString :: String -> String
fromString xs = evalState (eval (parseBrainFuckOp  xs)) ogTape


helloworld = fromString "++++++++++[>+>+++>+++++++>++++++++++<<<<-]>>>++.>+.+++++++..+++.<<++.>+++++++++++++++.>.+++.------.--------.<<+.<."

char = fromString ".+[.+]"


test = "+[+++++++++++++++++++++++++++++++++++>]<."

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import           Control.Monad.Trans.State
import           Data.Char
import           Debug.Trace
import           Prelude                           hiding (putChar)
import           System.Directory.Internal.Prelude (getArgs)

data Operator = Increase | Decrease | MoveRight | MoveLeft | PutChar | GetChar | Loop [Operator]
  deriving (Show, Eq)


data Tape a = Tape [a] a [a]

-- parseBrainFuckOp :: String -> [Operator]
-- parseBrainFuckOp (x:xs)

parseBrainFuckOp :: [Char] -> [Operator]
parseBrainFuckOp ('>':xs) = MoveRight : parseBrainFuckOp xs
parseBrainFuckOp ('<':xs) = MoveLeft  : parseBrainFuckOp xs
parseBrainFuckOp ('+':xs) = Increase :  parseBrainFuckOp xs
parseBrainFuckOp ('-':xs) = Decrease :  parseBrainFuckOp xs
parseBrainFuckOp ('.':xs) = PutChar :   parseBrainFuckOp xs
parseBrainFuckOp (',':xs) = GetChar :   parseBrainFuckOp xs
parseBrainFuckOp ('[':xs) = Loop (parseBrainFuckOp xsB) : parseBrainFuckOp xsA
    where   xsB = takeWhile (/=']') xs
            xsA = dropWhile (/=']') xs
parseBrainFuckOp (']':xs) = parseBrainFuckOp xs
parseBrainFuckOp _ = []

ogTape = Tape [] 0 [0| i <- [1..]]

eval :: [Operator] -> State (Tape Int) String
eval (Increase:xs) = do
                        y <- increase
                        ys <- eval xs
                        return $ y ++ ys
eval (Decrease:xs) = do
                        y <- decrease
                        ys <- eval xs
                        return $ y ++ ys
eval (PutChar:xs)  = do
                        -- traceM "puter "
                        y <- putChar
                        ys <- eval xs
                        return $ y : ys
eval (MoveRight:xs) = do
                        y <- moveRight
                        ys <- eval xs
                        return $ y ++ ys
eval (MoveLeft:xs) = do
                        y <- moveLeft
                        ys <- eval xs
                        return $ y ++ ys
eval ((Loop ops):xs) = do
                        y <- loop ops
                        ys <- eval xs
                        return $ y ++ ys

eval _             = return ""
-- eval (Increase:xs) = do increase
-- eval (Increase:xs) = do increase
-- eval (Increase:xs) = do increase
-- eval (Increase:xs) = do increase
-- eval (Increase:xs) = do increase


-- eval xs = do
--     -- put ogTape
--     increase
--     increase
--     increase
--     Tape sx x xs <- get

--     return $ show x


increase :: State (Tape Int) String
increase = do
    Tape sx x xs <- get
    let resultTape = Tape sx (mod (x + 1) 256) xs
    put resultTape
    return ""

decrease :: State (Tape Int) String
decrease = do
    Tape sx x xs <- get
    let resultTape = Tape sx (x - 1) xs
    put resultTape
    return ""

putChar :: State (Tape Int) Char
putChar = do
    traceM "put "
    Tape sx x xs <- get
    -- return $ chr (mod x 256)
    return $ chr x


moveRight :: State (Tape Int) String
moveRight = do
    Tape sx x xs <- get
    traceM "test"
    put $ Tape (sx ++[x]) (head xs) (tail xs)
    return ""

moveLeft :: State (Tape Int) String
moveLeft = do
    Tape sx x xs <- get
    if null sx
        then return ()
        else put $ Tape (init sx ) (last sx) (x: xs)
    return ""

loop :: [Operator] -> State (Tape Int) String
loop ys = do
    Tape _ x _ <- get
    -- if mod x 256 == 0
    if x == 0
    then return ""
    else do
        y <- eval ys
        ys <- loop ys
        return $ y ++ ys





main :: IO ()
main = do
    -- str <- getContents
    args <- getArgs
    let ops = parseBrainFuckOp $ concat args

    let (str,resultState) = runState (eval ops) ogTape

    putStrLn str


fromString :: String -> String
fromString xs = evalState (eval (parseBrainFuckOp  xs)) ogTape


helloworld = fromString "++++++++++[>+>+++>+++++++>++++++++++<<<<-]>>>++.>+.+++++++..+++.<<++.>+++++++++++++++.>.+++.------.--------.<<+.<."

char = fromString ".+[.+]"

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Main where
import System.Environment
import System.IO
import Control.Monad

import Text.Regex.TDFA

import Data.Char (isDigit)
import Data.List (isInfixOf)

--
-- Parsing functions / section
--

data TokenType t = WhiteSpace t | Quote t | Value t | Function t

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (head args)   -- read the contents of the input

    let sourceCode = rmNewLines contents
    let fNum = filter isDigit (head args)
    let output = "output-" ++ fNum ++ ".txt"    -- generate the output file Name
    print("Source: " ++ sourceCode)
    let res = tokenize sourceCode []

    print("O: " ++ show res)

    -- writeFile output res

rmNewLines :: String -> String
rmNewLines = map (\c -> if c == '\n' then ' ' else c)

tokenize :: [Char] -> [[Char]] -> [[Char]]
tokenize [] tokens = tokens
tokenize input tokens = 
    let match = tryParseOperator input    -- parse token
        newInput = rmPrefix match input   -- remove it from the source string
        nTokens = tokens ++ [match]       -- add the token to tokens
    in tokenize newInput nTokens          -- recursively call tokenize

-- 
tryParse :: RegexContext Regex t (String, String, String) => String -> t -> (t -> String) -> String
tryParse pattern sc nextFunc = 
    let (_, match, _) = sc =~ pattern :: (String, String, String)
    in case match of 
        "" -> nextFunc sc -- if its an empty match we call the next parse method!
        _ -> match   -- if matched we return the value itself

-- handles: ** + / - * %
tryParseOperator :: String -> String
tryParseOperator sc = tryParse "^(\\*\\*|\\+|\\/|\\-|\\*|%)" sc tryParseFloat 

tryParseFloat :: String -> String
tryParseFloat sc = tryParse "^-?[0-9]+\\.[0-9]+" sc tryParseInt

tryParseInt ::String -> String
tryParseInt sc = tryParse "^-?[1-9][0-9]*" sc tryRmPreWhiteSpace

-- This method will remove preceding whitespaces if they exist.
-- If they do then they are removed, and we go back to the operator parse method
-- However, if there isn't then no token was matched so an error should thrown!
tryRmPreWhiteSpace :: String -> String
tryRmPreWhiteSpace sc = 
    let res = tryParse "^[ \\t\\r\\n\\f]+" sc (\esc -> error ("Could not parse the token from source!\n'" ++ esc ++ "'"))
    in res

rmPrefix :: String -> String -> String
rmPrefix pre sc = drop (length pre) sc

--
-- Stack definition --
--
data Stack t = EmptyStack | Top t (Stack t) deriving (Eq, Show)
emptyStack :: Stack t
emptyStack = EmptyStack

isEmpty :: Stack t -> Bool
isEmpty EmptyStack = True
isEmpty _ = False

peek :: Stack t -> t
peek (Top top _) = top

push :: t -> Stack t -> Stack t
push top stack = Top top stack

pop :: Stack a -> Stack a
pop (Top _ stack) = stack

getFromTop :: Int -> Int -> Stack t -> t
getFromTop _ _ EmptyStack = error "Cannot 'get' from EmptyStack!"
getFromTop n cur (Top top stack)
    | cur == n = top
    | otherwise = getFromTop n (cur+1) stack

getAt :: Int -> Stack t -> t
getAt n stack = getFromTop n 1 stack


--
-- Value definition --
--


data Command = Add

-- 2.1 Core (55%)
-- A code grade of up to a “C” can be achieved by supporting
-- • integer operands, and basic integer operators (“+”, “*”, “-”, “/”).
-- • multiple operands and operators on a single line.
-- • more operators: exponentiation (“**”) and modulo (“%”).
-- • stack commands “DROP”, “DUP”, and “SWAP”.


--
-- Function definitions -- 
-- DUP DROP SWAP ROLL 
--
dup :: Stack t -> Stack t
dup EmptyStack = EmptyStack
dup stack = push (peek stack) stack

-- stack (s) drop can simply call pop!
sdrop :: Stack t -> Stack t
sdrop = pop

swap :: Stack t -> Stack t
swap (Top t1 (Top t2 stack)) = push t2 (push t1 stack)
swap _ = error "Cannot 'swap' a stack smaller than 2!"

-- push t onto swapped stack, then swap again
roll :: Stack t -> Stack t
roll (Top t stack) = swap(push t (swap stack))


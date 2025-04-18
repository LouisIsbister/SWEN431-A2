{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldl" #-}

module Main where
import System.Environment
import System.IO
import Control.Monad
import Text.Regex.Posix

import Data.Char (isDigit, toUpper, toLower)
import Data.List (isInfixOf, nub, sortBy)
import GHC.Float (int2Double, powerDouble)
import Data.Bits (Bits(xor, shiftL, shiftR, complement))
import Data.Fixed (mod')
import Data.Matrix (Matrix, fromLists)
import Data.Ord (comparing)

    ------------ 
    --- Todo --- 
    ------------

-- Challenge (30%)
-- For a very good grade, your solution needs to additionally support
-- [DONE] unary operators (“!” (not), ”˜” (one’s complement)).
-- [DONE] float operands and all respective float versions of the integer operators described in the Core and Completion sections.
-- • [DONE] vector and matrix operands.
-- • vector and matrix operators (“plus” (+), “times” (* (between matrices, or a matrix and a vector)), “dot”
-- (* (between vectors)), “cross” (x), “transpose” (TRANSP)).
-- • lambdas, [DONE] quoting, and the [DONE] “EVAL” command


    -------------------- 
    --- Main section --- 
    --------------------


main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (head args)   -- read the contents of the input

    let sourceCode = rmNewLines contents
    let fNum = filter isDigit (head args)
    let outFName = "output-" ++ fNum ++ ".txt"    -- generate the output file Name

    -- tokenise the input and execute the stack
    let tokens = tokenizeInput sourceCode
    print ("Tokens: " ++ tokensToString tokens)

    let outputStack = execute tokens emptyStack

    let stackAsList = stackToList outputStack
    let writeRes = map valueAsString stackAsList

    writeFile outFName (unlines writeRes)

-- string and token util functions 
rmNewLines :: String -> String
rmNewLines = map (\c -> if c == '\n' then ' ' else c)

upperFirst :: [Char] -> [Char]
upperFirst (x : xs) = toUpper x : xs

lowerFirst :: [Char] -> [Char]
lowerFirst (x : xs) = toLower x : xs

rmPrefix :: String -> String -> String
rmPrefix pre sc = drop (length pre) sc

tokensToString :: [StackElement] -> String
tokensToString [] = ""
tokensToString (x : xs) = valueAsString x ++ " " ++ tokensToString xs

-- assuming the strings we are dealing with have quotes around them!
addString :: String -> String -> String
addString l r = take (length l - 1) l ++ drop 1 r

mulitplyString :: Int -> String -> String
mulitplyString n str
    | n == 1 = str
    | otherwise = addString str (mulitplyString (n-1) str)

rmQuotes :: [Char] -> [Char]
rmQuotes str = drop 1 (take (length str - 1) str)



    -----------------------------------
    --- Parsing functions / section ---
    -----------------------------------


data StackElement =
    SDInt Int | SDDouble Double | SDString String | SDBool Bool |
    SDVector [StackElement] | SDMatrix [[StackElement]] |
    SDLambda Int [StackElement] | SDVar String |
    ManipFunc String | BinFunc String | UnaryFunc String |
    WhiteSpace String | Syntax String
    deriving (Eq, Show)

valueAsString :: StackElement -> String
valueAsString se = case se of
    (SDInt str) -> show str
    (SDDouble str) -> show str
    (SDString str) -> str
    (SDBool str) -> lowerFirst (show str)
    (SDVector arr) -> showSDVector arr
    (SDMatrix mat) -> showSDMatrix mat
    (SDLambda pCount tokens) -> showLambda pCount tokens
    (SDVar var) -> var
    (ManipFunc str) -> str
    (BinFunc str) -> str
    (UnaryFunc str) -> str
    (WhiteSpace str) -> str
    (Syntax str) -> str

showSDVector :: [StackElement] -> String
showSDVector vec = showSDVec vec "["

showSDVec :: [StackElement] -> String -> String
showSDVec [] acc = acc ++ "]"
showSDVec [x] acc = showSDVec [] (acc ++ valueAsString x)
showSDVec (x : xs) acc = showSDVec xs (acc ++ valueAsString x ++ ", ")

showLambda :: Int -> [StackElement] -> String
showLambda pc tokens = "{" ++ show pc ++ "|" ++ showSDVector tokens ++ "}"

showSDMatrix :: [[StackElement]] -> String
showSDMatrix mat = showSDMat mat "["

showSDMat :: [[StackElement]] -> String -> String
showSDMat [] acc = acc ++ "]"
showSDMat [x] acc = showSDMat [] (acc ++ showSDVector x)
showSDMat (vec : xs) acc = showSDMat xs (acc ++ showSDVector vec ++ ", ") 


tokenizeInput :: String -> [StackElement]
tokenizeInput input = createArrayPlusLambda (tokenize input [])

tokenize :: [Char] -> [StackElement] -> [StackElement]
tokenize [] tokens = tokens
tokenize input tokens =
    let match = tryParse input parsers   -- parse token
        matchStr = valueAsString match
        newInput = rmPrefix matchStr input   -- remove it from the source string

    -- recursively call tokenize, if its a whitespace, or comma ignore the token!
    in case match of
        WhiteSpace _ -> tokenize newInput tokens
        Syntax "," -> tokenize newInput tokens
        _ -> tokenize newInput (tokens ++ [match])


-- function stack takes the source code, tryies to match the top pattern,
-- converts the match to a stack element if it exists, otherwise go to the next pattern!
tryParse :: String -> [(String, String -> StackElement)] -> StackElement
tryParse sc [] = error ("Error! No token at start of: '" ++ sc ++ "'")
tryParse sc ((pattern, func) : xs) =
    let match = sc =~ pattern :: String
    in case match of
        "" -> tryParse sc xs      -- if its an empty match we call the next parse method!
        _ -> func match           -- otherwise return the value as a token

parsers :: [(String, String -> StackElement)]
parsers = [
    ("^(DROP|DUP|SWAP|ROT|ROLLD|ROLL|IFELSE|EVAL|SELF|TRANSP)", ManipFunc),
    ("^(\"[^\"]*\")", SDString),
    ("^(true|false)", SDBool . read . upperFirst),
    ("^(-?[0-9]+\\.[0-9]+)", SDDouble . read),
    ("^(0|-?[1-9][0-9]*)", SDInt . read),
    ("^(x[0-9]+)", SDVar),
    ("^(\\*\\*|\\+|\\/|\\-|\\*|%|\\^)", BinFunc),
    ("^(&|\\||==|!=|<<|>>|<=>|>=|<=|>|<|x)", BinFunc),
    ("^(!|~)", UnaryFunc),
    ("^(\\[|\\]|\\{|\\}|,|')", Syntax),
    ("^(\\s+)", WhiteSpace)
    ]

createArrayPlusLambda :: [StackElement] -> [StackElement]
createArrayPlusLambda tokens = parseArrayAndLambda tokens []

parseArrayAndLambda :: [StackElement] -> [StackElement] -> [StackElement]
parseArrayAndLambda [] seen = seen
parseArrayAndLambda ((Syntax "}") : tokens) seen = parseArrayAndLambda tokens (parseLambda (reverse seen))
parseArrayAndLambda ((Syntax "]") : tokens) seen = parseArrayAndLambda tokens (parseArray (reverse seen) [])
parseArrayAndLambda (t : tokens) seen = parseArrayAndLambda tokens (seen ++ [t])

parseArray :: [StackElement] -> [StackElement] -> [StackElement]
parseArray [] arrayBuilder = error ("Did not match [!" ++ show arrayBuilder)
parseArray ((Syntax "[") : xs) arrayBuilder = reverse (makeArrayLike arrayBuilder : xs)
parseArray (x : xs) arrayBuilder = parseArray xs (x : arrayBuilder)

makeArrayLike :: [StackElement] -> StackElement
makeArrayLike arr =
    if isMatrix arr
    then SDMatrix (map extractVector arr)
    else SDVector arr

extractVector :: StackElement -> [StackElement]
extractVector (SDVector xs) = xs

isMatrix :: [StackElement] -> Bool
isMatrix [] = False
isMatrix ((SDVector _) : _) = True
isMatrix (_ : xs) = isMatrix xs


parseLambda :: [StackElement] -> [StackElement]
parseLambda tokens = parseLambdaTokens tokens []

parseLambdaTokens :: [StackElement] -> [StackElement] -> [StackElement]
parseLambdaTokens (BinFunc "|" : SDInt paramCount : Syntax "{" : xs) tokenBuilder = reverse (SDLambda paramCount tokenBuilder : xs)
parseLambdaTokens (x : xs) tokenBuilder = parseLambdaTokens xs (x : tokenBuilder)


executeLambda :: StackElement -> Stack -> Stack
executeLambda (SDLambda paramCount tokens) stack =
    executeLambdaTokens
        (SDLambda paramCount tokens)
        tokens
        (popn stack paramCount)     -- ensure the variable values used to make the avr dictionary are popped off
        (makeVariableDictionary (getDistinctVars tokens) stack)    -- make variable dictionary

executeLambdaTokens :: StackElement -> [StackElement] -> Stack -> [(String, StackElement)] -> Stack
executeLambdaTokens _ [] stack _ = stack

executeLambdaTokens clone (Syntax "'" : x : xs) stack varDict =
    executeLambdaTokens clone xs (push x stack) varDict

executeLambdaTokens clone (ManipFunc "SELF" : xs) stack varDict =
    executeLambdaTokens clone xs (push clone stack) varDict

executeLambdaTokens clone (SDVar v : xs) stack varDict =
    executeLambdaTokens clone xs (push (getVarValue v varDict) stack) varDict

executeLambdaTokens clone (x : xs) stack varDict =
    executeLambdaTokens clone xs (executeToken x stack) varDict



getVarValue :: String -> [(String, StackElement)] -> StackElement
-- getVarValue varName d = error ("No value for: " ++ varName ++ " dict: " ++ show d)
getVarValue varName ((vName, value) : xs)
    | varName == vName = value
    | otherwise = getVarValue varName xs

-- makeVariableDictionary (getDistinctVars tokens)  stack
makeVariableDictionary :: [String] -> Stack -> [(String, StackElement)]
makeVariableDictionary [] _ = []
makeVariableDictionary (varName : tokens) (Top t stack) = (varName, t) : makeVariableDictionary tokens stack

-- list comprehension for gettign all distinct variable names
getDistinctVars :: [StackElement] -> [String]
getDistinctVars tokens = sortBy (flip (comparing getIndex)) (distinctVars tokens)

distinctVars :: [StackElement] -> [String]
distinctVars tokens = nub [varName | SDVar varName <- tokens]

getIndex :: String -> Int
getIndex varName = read (drop 1 varName)



    -------------------------------
    --- Stack execution seciton ---
    -------------------------------

-- take the list of tokens, executing each one as you go
execute :: [StackElement] -> Stack -> Stack
execute [] stack = stack
-- if the top is ' then simply push the token without execution!
execute (token : ts) (Top (Syntax "'") stack) = execute ts (push token stack)
execute (token : ts) stack = execute ts (executeToken token stack)

-- match the token and execute it
executeToken :: StackElement -> Stack -> Stack
executeToken token stack =
    case token of
        -- data - simply push it onto the stack
        SDInt _ -> push token stack
        SDDouble _ -> push token stack
        SDBool _ -> push token stack
        SDString _ -> push token stack
        SDVector _ -> push token stack
        SDMatrix _ -> push token stack
        SDLambda _ _ -> executeLambda token stack
        SDVar _ -> error "Handled by Lambda itself!"
        Syntax _ -> push token stack

        -- Stack manipulaion functions
        ManipFunc "DROP" -> pop stack    -- drop can simply call pop function!
        ManipFunc "DUP" -> dup stack
        ManipFunc "SWAP" -> swap stack
        ManipFunc "ROT" -> rot stack
        ManipFunc "ROLL" -> roll stack
        ManipFunc "ROLLD" -> rolld stack
        ManipFunc "IFELSE" -> ifelse stack
        ManipFunc "EVAL" -> eval stack
        ManipFunc "SELF" -> error "Handled by Lambda itself!"
        ManipFunc "TRANSP" -> transpose stack

        -- binary functions
        BinFunc "+" -> applyBinOp stack bAdd
        BinFunc "-" -> applyBinOp stack bSub
        BinFunc "*" -> applyBinOp stack bMul
        BinFunc "/" -> applyBinOp stack bDiv
        BinFunc "%" -> applyBinOp stack bMod
        BinFunc "**" -> applyBinOp stack bPow
        BinFunc "^" -> applyBinOp stack bXor
        BinFunc "<<" -> applyBinOp stack bShiftL
        BinFunc ">>" -> applyBinOp stack bShiftR

        BinFunc "&" -> applyBinOp stack bAnd
        BinFunc "|" -> applyBinOp stack bOr
        BinFunc "==" -> applyBinOp stack bEq
        BinFunc "!=" -> applyBinOp stack bNotEq
        BinFunc "<=>" -> applyBinOp stack bLogEq
        BinFunc ">=" -> applyBinOp stack bGtEq
        BinFunc "<=" -> applyBinOp stack bLtEq
        BinFunc ">" -> applyBinOp stack bGt
        BinFunc ">" -> applyBinOp stack bLt
        BinFunc "x" -> applyBinOp stack crossProduct

        -- unary functions
        UnaryFunc "!" -> applyUnaryOp stack uNot
        UnaryFunc "~" -> applyUnaryOp stack uComplement

        _ -> error "Unknown token!"


applyBinOp :: Stack -> (StackElement -> StackElement -> StackElement) -> Stack
applyBinOp (Top r (Top l stack)) func = push (func l r) stack

applyUnaryOp :: Stack -> (StackElement -> StackElement) -> Stack
applyUnaryOp (Top t stack) func = push (func t) stack

-- !!!!! check with Thomas should strings be able to eb added to each data type?
bAdd :: StackElement -> StackElement -> StackElement -- +
bAdd (SDString l) (SDString r) = SDString (addString l r)
bAdd (SDVector l) (SDVector r) = SDVector (zipWith bAdd l r)
bAdd l r = dispatchBinFunc (+) (+) l r

bSub :: StackElement -> StackElement -> StackElement -- -
bSub = dispatchBinFunc (-) (-)

bMul :: StackElement -> StackElement -> StackElement -- *
bMul (SDString str) (SDInt n) = SDString (mulitplyString n str)
bMul (SDVector l) (SDVector r) = vectorSum (zipWith bMul l r) -- multiply the vectors then sum the contents!
-- matrix function was sourced from https://chatgpt.com and adapted to use my custom methods: vectorSum, bMul, and transp
bMul (SDMatrix mat1) (SDMatrix mat2) = SDMatrix [[ vectorSum (zipWith bMul row col) | col <- transp mat2 ] | row <- mat1]
bMul (SDMatrix mat) (SDVector vec) = SDVector [vectorSum (zipWith bMul row vec) | row <- mat]
bMul l r = dispatchBinFunc (*) (*) l r

bDiv :: StackElement -> StackElement -> StackElement -- /
bDiv = dispatchBinFunc div (/)

-- !!!!! check with Thomas if mod should not work for doubles
bMod :: StackElement -> StackElement -> StackElement -- %
bMod (SDInt l) (SDInt r) = SDInt (l `mod` r)
bMod (SDDouble l) (SDDouble r) = SDDouble (l `mod'` r)

bPow :: StackElement -> StackElement -> StackElement -- **
bPow = dispatchBinFunc (^) powerDouble

bXor :: StackElement -> StackElement -> StackElement -- ^
bXor (SDBool l) (SDBool r) = SDBool (l `xor` r)
bXor (SDInt l) (SDInt r) = SDInt (l `xor` r)

bShiftL :: StackElement -> StackElement -> StackElement -- <<
bShiftL (SDInt l) (SDInt r) = SDInt (l `shiftL` r)

bShiftR :: StackElement -> StackElement -> StackElement -- >>
bShiftR (SDInt l) (SDInt r) = SDInt (l `shiftR` r)

-- takes 2 functions to apply to ints or doubles, also takes two stack 
-- elements and produces a new stack element
dispatchBinFunc :: (Int -> Int -> Int) -> (Double -> Double -> Double) -> StackElement -> StackElement -> StackElement
dispatchBinFunc ifunc _ (SDInt l) (SDInt r) = SDInt (ifunc l r)
dispatchBinFunc _ dfunc (SDInt l) (SDDouble r) = SDDouble (dfunc (int2Double l) r)
dispatchBinFunc _ dfunc (SDDouble l) (SDInt r) = SDDouble (dfunc l (int2Double r))
dispatchBinFunc _ dfunc (SDDouble l) (SDDouble r) = SDDouble (dfunc l r)


-- bool result functions <=> >= <= > <
bAnd :: StackElement -> StackElement -> StackElement -- &
bAnd (SDBool l) (SDBool r) = SDBool (l && r)

bOr :: StackElement -> StackElement -> StackElement -- |
bOr (SDBool l) (SDBool r) = SDBool (l || r)

bEq :: StackElement -> StackElement -> StackElement -- ==
bEq (SDString l) (SDString r) = SDBool (l == r)
bEq l r = dispatchBoolBinFunc (==) (==) l r

bNotEq :: StackElement -> StackElement -> StackElement -- !=
bNotEq (SDString l) (SDString r) = SDBool (l /= r)
bNotEq l r = dispatchBoolBinFunc (/=) (/=) l r

bGtEq :: StackElement -> StackElement -> StackElement -- >=
bGtEq = dispatchBoolBinFunc (>=) (>=)

bLtEq :: StackElement -> StackElement -> StackElement -- <=
bLtEq = dispatchBoolBinFunc (<=) (<=)

bGt :: StackElement -> StackElement -> StackElement -- >
bGt = dispatchBoolBinFunc (>) (>)

bLt :: StackElement -> StackElement -> StackElement -- <
bLt = dispatchBoolBinFunc (<) (<)

-- similar to other dispatch method, except this one hadnles boolean methods!
dispatchBoolBinFunc :: (Int -> Int -> Bool) -> (Double -> Double -> Bool) -> StackElement -> StackElement -> StackElement
dispatchBoolBinFunc ifunc _ (SDInt l) (SDInt r) = SDBool (ifunc l r)
dispatchBoolBinFunc _ dfunc (SDInt l) (SDDouble r) = SDBool (dfunc (int2Double l) r)
dispatchBoolBinFunc _ dfunc (SDDouble l) (SDInt r) = SDBool (dfunc l (int2Double r))
dispatchBoolBinFunc _ dfunc (SDDouble l) (SDDouble r) = SDBool (dfunc l r)

bLogEq :: StackElement -> StackElement -> StackElement -- <=>
bLogEq (SDInt l) (SDInt r) = SDInt (spaceship (int2Double l) (int2Double r))
bLogEq (SDDouble l) (SDDouble r) = SDInt (spaceship l r)
bLogEq _ _ = error "Cannot <=> on non-numeric types!"

spaceship :: Double -> Double -> Int
spaceship l r
    | l < r = -1
    | l == r = 0
    | otherwise = 1


-- Unary operators
uNot :: StackElement -> StackElement
uNot (SDBool x) = SDBool (not x)

uComplement :: StackElement -> StackElement
uComplement (SDInt x) = SDInt (complement x)
uComplement (SDBool x) = SDBool (not x)

vectorSum :: [StackElement] -> StackElement
vectorSum [x] = x
vectorSum (x : xs) = bAdd x (vectorSum xs)

crossProduct :: StackElement -> StackElement -> StackElement
crossProduct (SDVector [a1, a2, a3]) (SDVector [b1, b2, b3]) =
    SDVector [
        (a2 `bMul` b3) `bSub` (a3 `bMul` b2),
        (a3 `bMul` b1) `bSub` (a1 `bMul` b3),
        (a1 `bMul` b2) `bSub` (a2 `bMul` b1)
    ]

transpose :: Stack -> Stack
transpose (Top (SDMatrix mat) stack) = push (SDMatrix (transp mat)) stack

-- code was sourced from:
-- https://stackoverflow.com/questions/2578930/understanding-this-matrix-transposition-function-in-haskell
transp :: [[StackElement]] -> [[StackElement]]
transp ([]:_) = []
transp x = (map head x) : transp (map tail x)

    -------------------------
    ---  Stack definition ---
    -------------------------


data Stack = EmptyStack | Top StackElement Stack deriving (Eq, Show)

emptyStack :: Stack
emptyStack = EmptyStack

isEmpty :: Stack -> Bool
isEmpty EmptyStack = True
isEmpty _ = False

push :: StackElement -> Stack -> Stack
push = Top    -- same as:  push top stack = Top top stack

pop :: Stack -> Stack
pop stack = popn stack 1

popn :: Stack -> Int -> Stack
popn stack n = popNValues stack n 1

popNValues :: Stack -> Int -> Int -> Stack
popNValues (Top _ stack) n cur
    | n == cur = stack
    | otherwise = popNValues stack n (cur+1)

stackToList :: Stack -> [StackElement]
stackToList stack = sToList stack []

sToList :: Stack -> [StackElement] -> [StackElement]
sToList EmptyStack output = output
sToList (Top t stack) output = sToList stack (t:output)



    ---------------------------------------
    ---   Stack manipulation functions  ---
    ---------------------------------------
    ---   DROP DUP SWAP ROT ROLL ROLLD  ---
    ---           IFELSE EVAL           ---
    ---------------------------------------


dup :: Stack -> Stack
dup EmptyStack = EmptyStack
dup (Top t stack) = push t (push t stack)

swap :: Stack -> Stack
swap (Top t stack) = swapTop t stack

swapTop :: StackElement -> Stack -> Stack   -- define this methoid as its used for roll operations
swapTop sec (Top t stack) = push t (push sec stack)

-- push t onto swapped stack, then swap again
rot :: Stack -> Stack
rot (Top t stack) = swap (push t (swap stack))

roll :: Stack -> Stack
roll (Top (SDInt n) stack) = rollN stack n

rolld :: Stack -> Stack
rolld (Top (SDInt n) stack) = rolld_ stack n n

rolld_ :: Stack -> Int -> Int -> Stack -- we are calling rollN, n-1 times to effively roll in reverse!
rolld_ stack rollC 1 = stack
rolld_ stack rollC n = rolld_ (rollN stack rollC) rollC (n-1)

rollN :: Stack -> Int -> Stack
rollN (Top t1 stack) n
    | n <= 1 = push t1 stack
    | otherwise = swapTop t1 (rollN stack (n-1))

ifelse :: Stack -> Stack
ifelse (Top (SDBool cond) (Top r (Top l stack))) =
    if cond
        then push l stack
        else push r stack

eval :: Stack -> Stack
eval (Top t stack) = executeToken t stack

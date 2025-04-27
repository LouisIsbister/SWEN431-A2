{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldl" #-}

module Main where
import System.Environment (getArgs)
import System.IO ()
import Control.Monad ()

-- my imports
import Text.Regex.Posix ((=~))
import Data.Char (isDigit, toUpper, toLower)
import Data.List (nub, sortBy)
import GHC.Float (int2Double, powerDouble)
import Data.Bits (Bits(xor, shiftL, shiftR, complement))
import Data.Fixed (mod')
import Data.Ord (comparing, Down (Down))


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

    let tokens = tokenizeInput sourceCode -- tokenise the input -- print ("Tokens: " ++ tokensToString tokens)
    let outputStack = execute tokens EmptyStack -- execute the stack
    let stackAsList = stackToList outputStack
   
    let writeRes = map valueAsString stackAsList  -- get each StackElement as a string
    writeFile outFName (unlines writeRes)      -- write each elements with a newline on it


-- string utility functions 
rmNewLines :: String -> String
rmNewLines = map (\c -> if c == '\n' then ' ' else c)

upperFirst :: String -> String
upperFirst (x : xs) = toUpper x : xs

lowerFirst :: String -> String
lowerFirst (x : xs) = toLower x : xs

rmPrefix :: String -> String -> String
rmPrefix pre input = drop (length pre) input

-- assuming the strings we are dealing with have quotes around them!
addString :: String -> String -> String
addString l r = take (length l - 1) l ++ drop 1 r

mulitplyString :: Int -> String -> String
mulitplyString n str
    | n == 1 = str
    | otherwise = addString str (mulitplyString (n-1) str)



    ---------------------------------------------------------
    --- StackElement definition and custom show functions ---
    ---------------------------------------------------------

-- StackElement datatype, this allows us to represent the stack as 
-- Stack StackElement, i.e. Stack<StackElement>... in java! 
data StackElement =
    SDInt Int | SDDouble Double | SDString String | SDBool Bool |
    SDVector [StackElement] | SDMatrix [[StackElement]] |
    SDLambda Int [StackElement] | SDVar String |
    ManipFunc String | BinFunc String | UnaryFunc String |
    Syntax String | Ignorable String
    deriving (Eq, Show)

valueAsString :: StackElement -> String
valueAsString se = case se of
    (SDInt str) -> show str
    (SDDouble str) -> show str
    (SDString str) -> str
    (SDBool str) -> lowerFirst (show str)
    (SDVector arr) -> showSDVector arr
    (SDMatrix mat) -> showSDMatrix mat
    (SDLambda pc tokens) -> "{" ++ show pc ++ "|" ++ showSDVector tokens ++ "}"
    (SDVar var) -> var
    (ManipFunc str) -> str
    (BinFunc str) -> str
    (UnaryFunc str) -> str
    (Syntax str) -> str
    (Ignorable str) -> error "Ignorable should have been ignored!"

showSDVector :: [StackElement] -> String
showSDVector vec = showSDVec vec "["

showSDVec :: [StackElement] -> String -> String
showSDVec [] acc = acc ++ "]"
showSDVec [x] acc = showSDVec [] (acc ++ valueAsString x)
showSDVec (x : xs) acc = showSDVec xs (acc ++ valueAsString x ++ ", ")

showSDMatrix :: [[StackElement]] -> String
showSDMatrix mat = showSDMat mat "["

showSDMat :: [[StackElement]] -> String -> String
showSDMat [] acc = acc ++ "]"
showSDMat [x] acc = showSDMat [] (acc ++ showSDVector x)
showSDMat (vec : xs) acc = showSDMat xs (acc ++ showSDVector vec ++ ", ")

tokensToString :: [StackElement] -> String
tokensToString [] = ""
tokensToString (x : xs) = valueAsString x ++ " " ++ tokensToString xs


    ----------------------------------
    --- Parsing/Tokenizing section ---
    ----------------------------------

tokenizeInput :: String -> [StackElement]
tokenizeInput input = createArrayPlusLambda (tokenize input [])

tokenize :: String -> [StackElement] -> [StackElement]
tokenize [] tokens = tokens
tokenize input tokens =
    let (token, match) = tryParse input parsers   -- parse token
        newInput = rmPrefix match input   -- remove matched string from input

    -- recursively call tokenize, if its a whitespace or comma (Ignorable) ignore the token!
    in case token of
        Ignorable _ -> tokenize newInput tokens
        _ -> tokenize newInput (tokens ++ [token])

-- function takes the input, tries to match the top pattern. If it does match it
-- converts the match to a StackElement, otherwise go to the next pattern!
tryParse :: String -> [(String, String -> StackElement)] -> (StackElement, String)
tryParse input [] = error ("Error! No token at start of: '" ++ input ++ "'")
tryParse input ((pattern, func) : xs) =
    let match = input =~ pattern :: String
    in case match of
        "" -> tryParse input xs     -- if its an empty match we try parse the next pattern!
        _ -> (func match, match)    -- otherwise return a StackElement and match string

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
    ("^(\\[|\\]|\\{|\\}|')", Syntax),
    ("^(\\s+|,)", Ignorable)
    ]

createArrayPlusLambda :: [StackElement] -> [StackElement]
createArrayPlusLambda tokens = parseArrayAndLambda tokens []

-- when a closing sqaure bracket or brace is hit, we want to generate an array/matrix or lambda
-- we keep track of all the tokens 'seen' so far, we pass these 'seen' tokens to the parseLambda
-- and ParseArrya methods in reverse, this means the head of the seen list is the last element 
-- of an array, or last token of a lambda!
parseArrayAndLambda :: [StackElement] -> [StackElement] -> [StackElement]
parseArrayAndLambda [] seen = seen
parseArrayAndLambda ((Syntax "}") : tokens) seen = parseArrayAndLambda tokens (parseLambda (reverse seen))
parseArrayAndLambda ((Syntax "]") : tokens) seen = parseArrayAndLambda tokens (parseArray (reverse seen))
parseArrayAndLambda (t : tokens) seen = parseArrayAndLambda tokens (seen ++ [t]) -- append to the end of seen to maintain order

-- parsing arrays and matrices
parseArray :: [StackElement] -> [StackElement]
parseArray tokens = parseArrayTokens tokens []

-- generate a list fo tokens until the opening brace is hit. At this point create either 
-- an SDVector or SDMatrix and reverse the seen tokens again! 
parseArrayTokens :: [StackElement] -> [StackElement] -> [StackElement]
parseArrayTokens ((Syntax "[") : xs) arrayBuilder = reverse (makeArrayLike arrayBuilder : xs)
parseArrayTokens (x : xs) arrayBuilder = parseArrayTokens xs (x : arrayBuilder)

makeArrayLike :: [StackElement] -> StackElement
makeArrayLike arr
    | isMatrix arr = SDMatrix (map extractVector arr)
    | otherwise = SDVector arr

extractVector :: StackElement -> [StackElement]
extractVector (SDVector xs) = xs

isMatrix :: [StackElement] -> Bool
isMatrix [] = False
isMatrix ((SDVector _) : _) = True
isMatrix (_ : xs) = isMatrix xs

-- parsing lambdas
parseLambda :: [StackElement] -> [StackElement]
parseLambda tokens = parseLambdaTokens tokens []

parseLambdaTokens :: [StackElement] -> [StackElement] -> [StackElement]
parseLambdaTokens (BinFunc "|" : SDInt paramCount : Syntax "{" : xs) tokenBuilder = 
    reverse (SDLambda paramCount tokenBuilder : xs)
parseLambdaTokens (x : xs) tokenBuilder = parseLambdaTokens xs (x : tokenBuilder)


    -------------------------------
    --- Stack execution section ---
    -------------------------------

-- take the list of tokens, executing each one as you go
execute :: [StackElement] -> Stack -> Stack
execute [] stack = stack
execute (token : ts) (Top (Syntax "'") stack) = execute ts (push token stack) -- if top is ' quoted then push without execution!
execute (token : ts) stack = execute ts (executeToken token stack)   -- execute the top token of the stack

-- match the token and execute it
executeToken :: StackElement -> Stack -> Stack
executeToken token stack =
    case token of
        -- Stack manipulaion functions
        ManipFunc "DROP" -> pop stack    -- drop can simply call pop function!
        ManipFunc "DUP" -> dup stack
        ManipFunc "SWAP" -> swap stack
        ManipFunc "ROT" -> rot stack
        ManipFunc "ROLL" -> roll stack
        ManipFunc "ROLLD" -> rolld stack
        ManipFunc "IFELSE" -> ifelse stack
        ManipFunc "EVAL" -> eval stack
        ManipFunc "TRANSP" -> transpose stack
        ManipFunc "SELF" -> error "Handled by Lambda itself!"

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
        
        -- lambda execution
        SDLambda _ _ -> executeLambda token stack
        SDVar _ -> error "Handled by Lambda itself!"

        -- data tokens simply push onto the stack
        -- SDInt, SDDouble, SDBool, SDString, SDVector, SDMatrix, Syntax
        _ -> push token stack


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


-- vector and matrix specific operators
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


-- Lambda Execution!
executeLambda :: StackElement -> Stack -> Stack
executeLambda (SDLambda paramCount tokens) stack =
    execute
        (insertVarsAndRecursion paramCount tokens stack)
        (popn stack paramCount) -- remove the top elements from the stack as they now become values in the lambda 

insertVarsAndRecursion :: Int -> [StackElement] -> Stack-> [StackElement]
insertVarsAndRecursion paramCount tokens stack =
    updateLambda tokens
        (SDLambda paramCount tokens)   -- a clone of the lambda, used for recursive calls
        (makeVariableDictionary (distinctVariables tokens) stack)    -- dictionary of variable -> value

-- replace each variable with tis value, and replace each instance of SELF with the recursive clone!
updateLambda :: [StackElement] -> StackElement -> [(Int, StackElement)] -> [StackElement]
updateLambda [] _ _ = []
updateLambda (SDVar varName : ts)    clone varDict = getVarValue varName varDict : updateLambda ts clone varDict
updateLambda (ManipFunc "SELF" : ts) clone varDict = Syntax "'" : clone : updateLambda ts clone varDict -- quote the recursive call!
updateLambda (t : ts)                clone varDict = t : updateLambda ts clone varDict

getVarValue :: String -> [(Int, StackElement)] -> StackElement
getVarValue varName ((vNum, value) : xs)
    | getVarNumber varName == vNum = value
    | otherwise = getVarValue varName xs

-- make a map (list of tuples) of varNumber -> varValue!
makeVariableDictionary :: [Int] -> Stack -> [(Int, StackElement)]
makeVariableDictionary [] _ = []
makeVariableDictionary (varNum : tokens) (Top t stack) = (varNum, t) : makeVariableDictionary tokens stack

-- retrieve all distinct variable names, and sort them based upon their number!
distinctVariables :: [StackElement] -> [Int]
distinctVariables tokens =
    sortBy
        (comparing Down)  -- make largest variable at the start
        (nub [getVarNumber varName | SDVar varName <- tokens]) -- get unique vars

getVarNumber :: String -> Int
getVarNumber varName = read (drop 1 varName)


    -------------------------
    ---  Stack definition ---
    -------------------------

data Stack = EmptyStack | Top StackElement Stack deriving (Eq, Show)

push :: StackElement -> Stack -> Stack
push = Top    -- same as:  push top stack = Top top stack

pop :: Stack -> Stack
pop stack = popn stack 1

popn :: Stack -> Int -> Stack
popn stack n = popNHelper stack n 1

popNHelper :: Stack -> Int -> Int -> Stack
popNHelper (Top _ stack) n cur
    | n == cur = stack
    | otherwise = popNHelper stack n (cur+1)

stackToList :: Stack -> [StackElement]
stackToList stack = stackToListHelper stack []

stackToListHelper :: Stack -> [StackElement] -> [StackElement]
stackToListHelper EmptyStack output = output
stackToListHelper (Top t stack) output = stackToListHelper stack (t : output)


    ---------------------------------------
    ---   Stack manipulation functions  ---
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
rolld (Top (SDInt n) stack) = rolldHelper stack n n

rolldHelper :: Stack -> Int -> Int -> Stack -- we are calling rollN, n-1 times to effively roll in reverse!
rolldHelper stack rollC 1 = stack
rolldHelper stack rollC n = rolldHelper (rollN stack rollC) rollC (n-1)

rollN :: Stack -> Int -> Stack
rollN (Top t1 stack) n
    | n <= 1 = push t1 stack
    | otherwise = swapTop t1 (rollN stack (n-1))

ifelse :: Stack -> Stack
ifelse (Top (SDBool cond) (Top r (Top l stack)))
    | cond      = push l stack
    | otherwise = push r stack 

eval :: Stack -> Stack
eval (Top t stack) = executeToken t stack

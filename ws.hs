module Main where
import System.Environment (getArgs)
import System.IO ()
import Control.Monad ()

-- my imports
import Data.Bits (Bits(xor, shiftL, shiftR, complement))
import Data.Char (isDigit, toUpper, toLower)
import Data.Fixed (mod')
import Data.List (nub, sortBy, isPrefixOf, takeWhile, dropWhile)
import Data.Ord (comparing, Down (Down))

import GHC.Float (powerDouble)


    -------------------- 
    --- Main section --- 
    --------------------

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (head args)

    let sourceCode = rmNewLines contents
    let fNum = filter isDigit (head args)
    let outFName = "output-" ++ fNum ++ ".txt"    -- generate the output file Name

    -- token generation 
    let rawTokens = tokenize sourceCode -- tokenise the input
    let tokens = generateArraysAndLambdas rawTokens

    -- token and stack execution
    let outputStack = execute tokens EmptyStack
    let stackAsList = stackToList outputStack

    -- write results to file
    let writeRes = map show stackAsList   -- get each StackElement as a string
    writeFile outFName (unlines writeRes) -- write each elements with a newline on it

-- string utility functions 
rmNewLines :: String -> String
rmNewLines = map (\c -> if c == '\n' then ' ' else c)

upperFirst :: String -> String
upperFirst (x : xs) = toUpper x : xs

lowerFirst :: String -> String
lowerFirst (x : xs) = toLower x : xs

rmPrefix :: String -> String -> String
rmPrefix pre input = drop (length pre) input

-- assuming the strings we are dealing with have quotes around them! i.e "a" + "b" -> "ab"
addString :: String -> String -> String
addString l r = take (length l - 1) l ++ drop 1 r

mulitplyString :: Int -> String -> String
mulitplyString n str
    | n == 1 = str
    | otherwise = addString str (mulitplyString (n-1) str)

toDouble :: Int -> Double
toDouble = fromIntegral


    ---------------------------------------------------------
    --- StackElement definition and custom show functions ---
    ---------------------------------------------------------

-- StackElement datatype, this allows us to represent the stack as 
-- Stack StackElement, i.e. Stack<StackElement>... in java! 
-- Each one is prefixed with SD... for "stack deluxe"!
data StackElement = SDInt Int | SDDouble Double | SDString String | SDBool Bool |
    SDVector [StackElement] | SDMatrix [[StackElement]] |
    SDLambda Int [StackElement] | SDVar String |           -- lambda types
    ManipFunc String | BinFunc String | UnaryFunc String | -- function types
    Syntax String | Ignorable |
    ParseFail
    deriving Eq

instance Show StackElement where
    show :: StackElement -> String
    show (SDInt x) = show x
    show (SDDouble x) = show x
    show (SDBool b) = lowerFirst (show b)
    show (SDVector vec) = showSDVector vec
    show (SDMatrix mat) = showSDMatrix mat
    show (SDLambda paramCount tokens) = "{" ++ show paramCount ++ "|" ++ showSDVector tokens ++ "}"

    show (SDVar str) = str     ; show (SDString str) = str
    show (ManipFunc str) = str ; show (BinFunc str) = str
    show (UnaryFunc str) = str ; show (Syntax str) = str

    show Ignorable = error "Ignorable should have been ignored by tokenizer!"
    show ParseFail = error "ParseFail detected. Parsing process failed to identify a token!"

-- functions for displying a SDVector
showSDVector :: [StackElement] -> String
showSDVector vec = showSDVec vec "["

showSDVec :: [StackElement] -> String -> String
showSDVec [] acc = acc ++ "]"
showSDVec [x] acc = showSDVec [] (acc ++ show x)
showSDVec (x : xs) acc = showSDVec xs (acc ++ show x ++ ", ")

-- functions for displying a SDMatrix!
showSDMatrix :: [[StackElement]] -> String
showSDMatrix mat = showSDMat mat "["

showSDMat :: [[StackElement]] -> String -> String
showSDMat [] acc = acc ++ "]"
showSDMat [x] acc = showSDMat [] (acc ++ showSDVector x)
showSDMat (vec : xs) acc = showSDMat xs (acc ++ showSDVector vec ++ ", ")


    ----------------------------------
    --- Parsing/Tokenizing section ---
    ----------------------------------

-- methods to iterate through the input string, generating tokens until it
-- is it empty
tokenize :: String -> [StackElement]
tokenize input = tokenizeHelper input []

tokenizeHelper :: String -> [StackElement] -> [StackElement]
tokenizeHelper [] tokens = tokens
tokenizeHelper input tokens =
    let (token, newInput) = tryParse input parsers   -- parse token
    in case token of
        Ignorable -> tokenizeHelper newInput tokens
        _         -> tokenizeHelper newInput (tokens ++ [token])

-- function takes the input, tries to match the top pattern. If it does match it
-- converts the match to a StackElement, otherwise go to the next pattern!
tryParse :: String -> [String -> (String, StackElement)] -> (StackElement, String)
tryParse input (parser : ps) =
    let (newInput, token) = parser input
    in case token of
        ParseFail -> tryParse input ps
        _         -> (token, newInput)

-- return a list of functions, each takes an input string, tries to match the start 
-- of that string to a specific pattern. If successful, remove the match from the start
-- of the input and return a tuple with the new input and the StackElement generated
-- form the match. If it fails it will always return the orginal input and a ParseFail!
parsers :: [String -> (String, StackElement)]
parsers = [
        parseManipFunc,
        parseString,
        parseBool, parseNumeric, parseVar,
        parseBinFunc, parseUnaryFunc,
        parseSyntax, parseIgnorable
    ]

-- parsing of stack manipulation functions 
parseManipFunc :: String -> (String, StackElement)
parseManipFunc input = parseManipF input ["DROP","DUP","SWAP","ROT","ROLLD","ROLL","IFELSE","EVAL","SELF","TRANSP"]

parseManipF :: String -> [String] -> (String, StackElement)
parseManipF input [] = (input, ParseFail)
parseManipF input (mp : mps)
    | mp `isPrefixOf` input = (drop (length mp) input, ManipFunc mp)
    | otherwise = parseManipF input mps

-- String parsing, if the first char is " then take evything until next "!
parseString :: [Char] -> (String, StackElement)
parseString (firstChar : rest)
    | firstChar == '\"' = handleString rest (takeWhile (/= '\"') rest)
    | otherwise = (firstChar : rest, ParseFail)

handleString :: [Char] -> [Char] -> (String, StackElement)
handleString input captured = (drop (length captured + 1) input, SDString ("\"" ++ captured ++ "\""))

-- parse boolean
parseBool :: String -> (String, StackElement)
parseBool input
    | "true" `isPrefixOf` input = (drop 4 input, SDBool True)
    | "false" `isPrefixOf` input = (drop 5 input, SDBool False)
    | otherwise = (input, ParseFail)

-- parse numeric function checks to see if the first char is an int. If so, get it
-- and then call the parse floating point function which will return a SDDouble if
-- the next char is a '.', otherwise it will just return an SDInt with the number matched at first
parseNumeric :: String -> (String, StackElement)
parseNumeric (firstChar : rest)
    | isDigit firstChar =
        let (captureLen, num) = parseNumber rest
            newRest = drop captureLen rest
        in parseFloatingPoint (firstChar : num) newRest
    | firstChar == '-' =
        case parseNumeric rest of
            (newRest, SDInt x)    -> (newRest, SDInt (-x))
            (newRest, SDDouble x) -> (newRest, SDDouble (-x))
            (_, ParseFail)        -> ('-' : rest, ParseFail)
    | otherwise = (firstChar : rest, ParseFail)

parseFloatingPoint :: String -> String -> (String, StackElement)
parseFloatingPoint wholeNum (firstChar : rest)
    | firstChar == '.' =
        let (captureLen, decimal) = parseNumber rest
            newRest = drop captureLen rest
        in if captureLen == 0
            then (newRest, SDDouble (read wholeNum :: Double)) -- if the .0 is missing, i.e. 123. instead of 123.0
            else (newRest, SDDouble (read (wholeNum ++ "." ++ decimal)))
    | otherwise = (firstChar : rest, SDInt (read wholeNum)) -- default is an int as it has already been parsd

parseNumber :: String -> (Int, String)  -- while there are digits take them, return the length of the captured num and its string
parseNumber input =
    let capturedNum = takeWhile isDigit input
    in (length capturedNum, capturedNum)

-- try parse a variable!
parseVar :: String -> (String, StackElement)
parseVar input
    | "x" `isPrefixOf` input =
        let (numLen, num) = parseNumber (tail input)  -- retrieve digits after the 'x' of a variable
        in if numLen == 0   -- if there were none then it was a cross prodcut and we should fail!
            then (input, ParseFail)
            else (drop (numLen + 1) input, SDVar ('x' : num))
    | otherwise = (input, ParseFail)

-- parsing binary oprators
parseBinFunc :: String -> (String, StackElement)
parseBinFunc input =
    parseBinF input ["**", "+", "/", "-", "*", "%", "^", "&", "|", "==", "!=", "<<", ">>", "<=>", ">=", "<=", ">", "<", "x"]

parseBinF :: String -> [String] -> (String, StackElement)
parseBinF input [] = (input, ParseFail)
parseBinF input (bf : bfs)
    | bf `isPrefixOf` input = (drop (length bf) input, BinFunc bf)
    | otherwise = parseBinF input bfs

-- unary function parsing
parseUnaryFunc :: String -> (String, StackElement)
parseUnaryFunc (firstChar : rest)
    | firstChar == '!' = (rest, UnaryFunc "!")
    | firstChar == '~' = (rest, UnaryFunc "~")
    | otherwise = (firstChar : rest, ParseFail)

-- parse each piece of syntax
parseSyntax :: String -> (String, StackElement)
parseSyntax input = parseSyn input ["[", "]", "{", "}", "'"]

parseSyn :: String -> [String] -> (String, StackElement)
parseSyn input [] = (input, ParseFail)
parseSyn input (s : ss)
    | s `isPrefixOf` input = (drop 1 input, Syntax s)
    | otherwise = parseSyn input ss

-- parse tokens that will be discarded
parseIgnorable :: String -> (String, StackElement)
parseIgnorable (firstChar : rest)
    | firstChar == ',' = (rest, Ignorable)
    | firstChar == ' ' = (dropWhile (== ' ') rest, Ignorable)
    | otherwise = (firstChar : rest, ParseFail)


-- Parsing Lambdas, Vectors, & Matrices
generateArraysAndLambdas :: [StackElement] -> [StackElement]
generateArraysAndLambdas tokens = parseArrayAndLambda tokens []

-- when a closing sqaure bracket or brace is hit, we want to generate an array/matrix or lambda
-- we keep track of all the tokens 'seen' so far, we pass these 'seen' tokens to the parseLambda
-- and ParseArrya methods in reverse, this means the head of the seen list is the last element 
-- of an array, or last token of a lambda!
parseArrayAndLambda :: [StackElement] -> [StackElement] -> [StackElement]
parseArrayAndLambda [] seen = seen
parseArrayAndLambda ((Syntax "}") : tokens) seen =
    parseArrayAndLambda tokens (parseLambda (reverse seen))
parseArrayAndLambda ((Syntax "]") : tokens) seen =
    parseArrayAndLambda tokens (parseArray (reverse seen))
parseArrayAndLambda (t : tokens) seen = parseArrayAndLambda tokens (seen ++ [t]) -- append to the end of seen to maintain order

-- generate a list of tokens until the opening brace is hit. At this point create either 
-- an SDVector or SDMatrix and reverse the seen tokens again! 
parseArray :: [StackElement] -> [StackElement]
parseArray tokens = reverse (parseArrayTokens tokens [])

parseArrayTokens :: [StackElement] -> [StackElement] -> [StackElement]
parseArrayTokens ((Syntax "[") : xs) arrayBuilder = makeArrayLike arrayBuilder : xs
parseArrayTokens (x : xs) arrayBuilder = parseArrayTokens xs (x : arrayBuilder)

-- if the array has vecs as args, then it must be a matrix!
makeArrayLike :: [StackElement] -> StackElement
makeArrayLike arr
    | isMatrix arr = SDMatrix [vec | (SDVector vec) <- arr] -- extract all the vectors from the SDVectors
    | otherwise = SDVector arr

isMatrix :: [StackElement] -> Bool -- if the first element of an array is a vector it must be a Matrix
isMatrix ((SDVector _) : _) = True
isMatrix _ = False

-- parsing lambdas
parseLambda :: [StackElement] -> [StackElement]
parseLambda tokens = reverse (parseLambdaTokens tokens [])

parseLambdaTokens :: [StackElement] -> [StackElement] -> [StackElement]
parseLambdaTokens (BinFunc "|" : SDInt paramX : Syntax "{" : xs) lTokens = SDLambda paramX lTokens : xs
parseLambdaTokens (x : xs) lTokens = parseLambdaTokens xs (x : lTokens)


    -------------------------------
    --- Stack execution section ---
    -------------------------------

-- take the list of tokens, executing each one as you go. If the top of 
-- the stack is quoted then push the token without execution. Otherwise,
-- execute the next token!
execute :: [StackElement] -> Stack -> Stack
execute [] stack = stack
execute (token : ts) (Top (Syntax "'") stack) = execute ts (push token stack)
execute (token : ts) stack = execute ts (executeToken token stack)

-- match the token and execute it
executeToken :: StackElement -> Stack -> Stack
executeToken token stack =
    case token of
        -- Stack manipulaion functions
        ManipFunc "DROP"   -> pop stack
        ManipFunc "DUP"    -> dup stack
        ManipFunc "SWAP"   -> swap stack
        ManipFunc "ROT"    -> rot stack
        ManipFunc "ROLL"   -> roll stack
        ManipFunc "ROLLD"  -> rolld stack
        ManipFunc "IFELSE" -> ifelse stack
        ManipFunc "EVAL"   -> eval stack
        ManipFunc "TRANSP" -> transpose stack
        -- binary functions
        BinFunc "+"   -> applyBinOp stack bAdd
        BinFunc "-"   -> applyBinOp stack bSub
        BinFunc "*"   -> applyBinOp stack bMul
        BinFunc "/"   -> applyBinOp stack bDiv
        BinFunc "%"   -> applyBinOp stack bMod
        BinFunc "**"  -> applyBinOp stack bPow
        BinFunc "^"   -> applyBinOp stack bXor
        BinFunc "<<"  -> applyBinOp stack bShiftL
        BinFunc ">>"  -> applyBinOp stack bShiftR
        BinFunc "&"   -> applyBinOp stack bAnd
        BinFunc "|"   -> applyBinOp stack bOr
        BinFunc "=="  -> applyBinOp stack bEq
        BinFunc "!="  -> applyBinOp stack bNotEq
        BinFunc "<=>" -> applyBinOp stack bLogEq
        BinFunc ">="  -> applyBinOp stack bGtEq
        BinFunc "<="  -> applyBinOp stack bLtEq
        BinFunc ">"   -> applyBinOp stack bGt
        BinFunc ">"   -> applyBinOp stack bLt
        BinFunc "x"   -> applyBinOp stack crossProduct
        -- unary functions
        UnaryFunc "!" -> applyUnaryOp stack uNot
        UnaryFunc "~" -> applyUnaryOp stack uComplement
        -- lambda execution
        SDLambda _ _     -> executeLambda token stack
        ManipFunc "SELF" -> error "Handled by Lambda itself!"
        SDVar _          -> error "Handled by Lambda itself!"
        -- data tokens simply push onto the stack
        -- SDInt, SDDouble, SDBool, SDString, SDVector, SDMatrix, Syntax
        _ -> push token stack

-- given the stack and a binary function, apply the function to the top two 
-- stack elements, and push the result onto the remainder of the stack
applyBinOp :: Stack -> (StackElement -> StackElement -> StackElement) -> Stack
applyBinOp (Top r (Top l stack)) func = push (func l r) stack

-- much the same, given the stack and a unary function, apply the function 
-- to the top-most stack element, pushing the result onto the remainder of the stack
applyUnaryOp :: Stack -> (StackElement -> StackElement) -> Stack
applyUnaryOp (Top t stack) func = push (func t) stack

-- This function can be called when a binary function is applied to numeric stack elements. 
-- its takes to functions as args, one to handle integer operations, and to handle double operations.
-- if both args are ints, it will apply the integer function and return an SDInt
-- otherwise, one (or both) of the args are doubles, and should return a SDDouble!
dispatchNumBinFunc :: (Int -> Int -> Int)
    -> (Double -> Double -> Double)
    -> StackElement
    -> StackElement
    -> StackElement
dispatchNumBinFunc ifunc _ (SDInt l) (SDInt r) = SDInt (ifunc l r)
dispatchNumBinFunc _ dfunc (SDInt l) (SDDouble r) = SDDouble (dfunc (toDouble l) r)
dispatchNumBinFunc _ dfunc (SDDouble l) (SDInt r) = SDDouble (dfunc l (toDouble r))
dispatchNumBinFunc _ dfunc (SDDouble l) (SDDouble r) = SDDouble (dfunc l r)

-- Exactly the same as the previous method, except this one handles the binary functiosn that 
-- take two numbers and return booleans instead of numbers again!
dispatchBoolBinFunc :: (Int -> Int -> Bool)
    -> (Double -> Double -> Bool)
    -> StackElement
    -> StackElement
    -> StackElement
dispatchBoolBinFunc ifunc _ (SDInt l) (SDInt r) = SDBool (ifunc l r)
dispatchBoolBinFunc _ dfunc (SDInt l) (SDDouble r) = SDBool (dfunc (toDouble l) r)
dispatchBoolBinFunc _ dfunc (SDDouble l) (SDInt r) = SDBool (dfunc l (toDouble r))
dispatchBoolBinFunc _ dfunc (SDDouble l) (SDDouble r) = SDBool (dfunc l r)


-- binary addition
bAdd :: StackElement -> StackElement -> StackElement
bAdd (SDString l) (SDString r) = SDString (addString l r) -- String addition
bAdd (SDString l) other = SDString (addString l ("\"" ++ show other ++ "\""))
bAdd other (SDString r) = SDString (addString ("\"" ++ show other ++ "\"") r)
bAdd (SDVector l) (SDVector r) = SDVector (zipWith bAdd l r) -- vector addition
bAdd l r = dispatchNumBinFunc (+) (+) l r  -- all other addition

-- binary subtraction
bSub :: StackElement -> StackElement -> StackElement
bSub l r = dispatchNumBinFunc (-) (-) l r

-- binary multiplication
bMul :: StackElement -> StackElement -> StackElement
bMul (SDString str) (SDInt n) = SDString (mulitplyString n str)
bMul (SDVector l) (SDVector r) = vectorSum (zipWith bMul l r) -- multiply the vectors then sum the contents!
-- matrix function was sourced from https://chatgpt.com and adapted to use my custom methods: vectorSum, bMul, and transp
bMul (SDMatrix mat1) (SDMatrix mat2) = SDMatrix [[ vectorSum (zipWith bMul row col) | col <- transp mat2 ] | row <- mat1]
bMul (SDMatrix mat) (SDVector vec) = SDVector [vectorSum (zipWith bMul row vec) | row <- mat]
bMul l r = dispatchNumBinFunc (*) (*) l r

-- binary division
bDiv :: StackElement -> StackElement -> StackElement
bDiv = dispatchNumBinFunc div (/)

-- binary modulus operation
bMod :: StackElement -> StackElement -> StackElement
bMod (SDInt l) (SDInt r) = SDInt (l `mod` r)
bMod (SDDouble l) (SDDouble r) = SDDouble (l `mod'` r)

-- binary exponentiation operation
bPow :: StackElement -> StackElement -> StackElement
bPow l r = dispatchNumBinFunc (^) powerDouble l r

-- binary XOR
bXor :: StackElement -> StackElement -> StackElement
bXor (SDBool l) (SDBool r) = SDBool (l `xor` r)
bXor (SDInt l) (SDInt r) = SDInt (l `xor` r)

-- binary shift left
bShiftL :: StackElement -> StackElement -> StackElement
bShiftL (SDInt l) (SDInt r) = SDInt (l `shiftL` r)

-- binary shift right
bShiftR :: StackElement -> StackElement -> StackElement
bShiftR (SDInt l) (SDInt r) = SDInt (l `shiftR` r)

-- logical equality i.e. spaceship operator <=>!
bLogEq :: StackElement -> StackElement -> StackElement
bLogEq (SDInt l) (SDInt r) = SDInt (spaceship (toDouble l) (toDouble r))
bLogEq (SDDouble l) (SDDouble r) = SDInt (spaceship l r)
bLogEq _ _ = error "Cannot <=> on non-numeric types!"

spaceship :: Double -> Double -> Int
spaceship l r
    | l < r = -1
    | l == r = 0
    | otherwise = 1


-- boolean and
bAnd :: StackElement -> StackElement -> StackElement
bAnd (SDBool l) (SDBool r) = SDBool (l && r)

-- boolean or
bOr :: StackElement -> StackElement -> StackElement
bOr (SDBool l) (SDBool r) = SDBool (l || r)

-- boolean equals
bEq :: StackElement -> StackElement -> StackElement
bEq (SDBool l) (SDBool r) = SDBool (l == r)
bEq (SDString l) (SDString r) = SDBool (l == r)
bEq l r = dispatchBoolBinFunc (==) (==) l r

-- boolean does not equal
bNotEq :: StackElement -> StackElement -> StackElement
bNotEq (SDBool l) (SDBool r) = SDBool (l /= r)
bNotEq (SDString l) (SDString r) = SDBool (l /= r)
bNotEq l r = dispatchBoolBinFunc (/=) (/=) l r

-- boolean greater than or equal to
bGtEq :: StackElement -> StackElement -> StackElement
bGtEq = dispatchBoolBinFunc (>=) (>=)

-- boolean less than or equal to
bLtEq :: StackElement -> StackElement -> StackElement
bLtEq = dispatchBoolBinFunc (<=) (<=)

-- boolean greater than
bGt :: StackElement -> StackElement -> StackElement
bGt = dispatchBoolBinFunc (>) (>)

-- boolean less than
bLt :: StackElement -> StackElement -> StackElement
bLt = dispatchBoolBinFunc (<) (<)


-- Unary operators
uNot :: StackElement -> StackElement
uNot (SDBool x) = SDBool (not x)

uComplement :: StackElement -> StackElement
uComplement (SDInt x) = SDInt (complement x)
uComplement (SDBool x) = SDBool (not x)


-- sum each StackElement in a vector of StackElements 
vectorSum :: [StackElement] -> StackElement
vectorSum [x] = x
vectorSum (x : xs) = bAdd x (vectorSum xs)

-- cross product of SDVectors
crossProduct :: StackElement -> StackElement -> StackElement
crossProduct (SDVector [a1, a2, a3]) (SDVector [b1, b2, b3]) =
    SDVector [
        (a2 `bMul` b3) `bSub` (a3 `bMul` b2),
        (a3 `bMul` b1) `bSub` (a1 `bMul` b3),
        (a1 `bMul` b2) `bSub` (a2 `bMul` b1)
    ]

transpose :: Stack -> Stack
transpose (Top (SDMatrix mat) stack) = push (SDMatrix (transp mat)) stack

-- Transpose code was sourced from the below link:
-- https://stackoverflow.com/questions/2578930/understanding-this-matrix-transposition-function-in-haskell
transp :: [[StackElement]] -> [[StackElement]]
transp ([]:_) = []
transp x = (map head x) : transp (map tail x)


    -------------------------
    --- Lambda Execution! ---
    -------------------------

-- takes a lambda and the Stack, calls the 'execute' method as each lambda is effectively just a stack! 
executeLambda :: StackElement -> Stack -> Stack
executeLambda (SDLambda paramCount tokens) stack =
    execute
        (updateLambdaTokens paramCount tokens stack)
        (popn stack paramCount) -- pop the stack elements that were passed as variables to the lambda! 

-- replace all lambda var and SELF tokens with their respective values, and return the resulting tokens!
updateLambdaTokens :: Int -> [StackElement] -> Stack-> [StackElement]
updateLambdaTokens paramCount tokens stack =
    replaceVars
        (replaceSELF tokens (SDLambda paramCount tokens))    -- first replace all SELF's with the recursive call!
        (makeVarDictionary (distinctVariables tokens) stack) -- generate list of (SDVar, StackElement) tuples

-- replaces all occurrences of SELF with a clone of the original lambda!
replaceSELF :: [StackElement] -> StackElement -> [StackElement]
replaceSELF [] _ = []
replaceSELF (ManipFunc "SELF" : ts) clone = Syntax "'" : clone : replaceSELF ts clone
replaceSELF (t : ts) clone                = t : replaceSELF ts clone

-- replaces all variables with thei respective value!
replaceVars :: [StackElement] -> [(StackElement, StackElement)] -> [StackElement]
replaceVars [] _ = []
replaceVars (SDVar varName : ts) varDict = getVarValue varName varDict : replaceVars ts varDict
replaceVars (t : ts) varDict             = t : replaceVars ts varDict

-- given a var name, and var 'dictionary', return the value of the var
getVarValue :: String -> [(StackElement, StackElement)] -> StackElement
getVarValue varName ((SDVar targetVar, value) : xs)
    | varName == targetVar = value
    | otherwise            = getVarValue varName xs

-- make a map (list of tuples) of variable -> value!
makeVarDictionary :: [StackElement] -> Stack -> [(StackElement, StackElement)]
makeVarDictionary [] _ = []
makeVarDictionary (var : tokens) (Top t stack) = (var, t) : makeVarDictionary tokens stack

-- retrieves all unique variables, sorts them in descending order by their number! 
distinctVariables :: [StackElement] -> [StackElement]
distinctVariables tokens = 
    sortBy              -- 'flip' sorts in descending order i.e. x3 -> x2 -> x1 -> x0 
        (flip (\(SDVar a) (SDVar b) -> compare (getVarNumber a) (getVarNumber b))) 
        (nub [SDVar varName | SDVar varName <- tokens]) -- retrieve all variables

-- simply remove the 'x' from teh variable name, and convert to an Int
getVarNumber :: String -> Int
getVarNumber varName = read (drop 1 varName)


    -------------------------
    ---  Stack definition ---
    -------------------------

data Stack = EmptyStack | Top StackElement Stack
    deriving (Eq, Show)

push :: StackElement -> Stack -> Stack
push top stack = Top top stack

pop :: Stack -> Stack
pop stack = popn stack 1

popn :: Stack -> Int -> Stack
popn (Top t stack) n
    | n == 0 = push t stack  -- ensures t is not lost if 0 is passed!
    | n == 1 = stack
    | otherwise = popn stack (n-1)

stackToList :: Stack -> [StackElement]
stackToList stack = stackToListHelper stack []

stackToListHelper :: Stack -> [StackElement] -> [StackElement]
stackToListHelper EmptyStack output = output
stackToListHelper (Top t stack) output = stackToListHelper stack (t : output)

-- Stack manipulation functions

dup :: Stack -> Stack
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
rolld (Top (SDInt n) stack) = rollNMinus1Times stack n (n-1)

-- Call rollN on the stack n-1 times to effively roll in reverse!
rollNMinus1Times :: Stack -> Int -> Int -> Stack
rollNMinus1Times stack n rollsLeft
    | rollsLeft == 0 = stack   -- if each ROLL has been completed 
    | otherwise      = rollNMinus1Times (rollN stack n) n (rollsLeft-1)

rollN :: Stack -> Int -> Stack
rollN (Top t1 stack) n
    | n <= 1 = push t1 stack
    | otherwise = swapTop t1 (rollN stack (n-1))

ifelse :: Stack -> Stack
ifelse (Top (SDBool cond) (Top f (Top t stack)))
    | cond      = push t stack
    | otherwise = push f stack

eval :: Stack -> Stack
eval (Top t stack) = executeToken t stack

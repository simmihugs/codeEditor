module CLexer where

data TokenType = TokMisc
               | TokType
               | TokIdent
               | TokKeyword
               | TokOperator
               | TokInt
               | TokChar
               | TokString
               deriving (Show)

data Token = Token {
  tokenType :: TokenType
  , valueString :: String
  } deriving Show

cMisc :: String
cMisc = "(){}[];,"

cIdentFirst :: String
cIdentFirst = '_':['a'..'z'] ++ ['A'..'Z']

cIdentChar :: String
cIdentChar = cIdentFirst ++ ['0'..'9']

cKeywords :: [String]
cKeywords = ["if","while","return"]--"else","__syscall"

cTypes :: [String]
cTypes = ["int", "char"]

cOperatorStart :: String
cOperatorStart = "=><*/&!+-"--"^%~|.?:"

cOperators :: [String]
cOperators = ["=" ,"==",">" ,"<"
             ,"<=",">=","!=","*"
             ,"/" ,"+" ,"-" ,"!","&"]

-- strings!
lexParseString :: String -> String -> [Token]
lexParseString t [] = error "Lexer couldn't parse string literal"
lexParseString tokStr (x:xs)
    | x == '\\'             = lexParseString (tokStr ++ [head xs]) (tail xs)
    | x == '\"'             = Token TokString tokStr:lexer xs
    | otherwise             = lexParseString (tokStr ++ [x]) xs


-- operators
-- 1-2 characters only, so we just handle those cases
lexParseOperator :: Char -> String -> [Token]
lexParseOperator t [] = error "Lexer couldn't parse operator"
lexParseOperator tokChar (x:xs)
    | [tokChar,x] `elem` cOperators = Token TokOperator [tokChar,x]:lexer xs
    | otherwise                     = Token TokOperator [tokChar]:lexer (x:xs)

-- chars - give this a string starting with the char, as in "a'"
lexParseChar :: String -> [Token]
lexParseChar [] = error "Lexer couldn't parse char literal"
lexParseChar [t] = error "Lexer couldn't parse char literal"
lexParseChar (x:xs) = if head xs == '\''
                        then Token TokChar [x] : lexer (tail xs)
                        else error "Lexer couldn't parse char literal"

-- integers, including hex and octal, and doubles
-- TODO doubles
lexParseNum :: String -> String -> [Token]
lexParseNum t [] = error "Lexer couldn't parse numeric literal"
lexParseNum tokStr (x:xs)
    | x `elem` ['0'..'9']   = lexParseNum (tokStr ++ [x]) xs
    | otherwise             = Token TokInt tokStr:lexer (x:xs)

-- text tokens - identifiers, types, keywords
lexParseText :: String -> String -> [Token]
lexParseText t [] = error "Lexer couldn't parse identifier"
lexParseText tokStr (x:xs)
    | x `elem` cIdentChar     = lexParseText (tokStr ++ [x]) xs
    | tokStr `elem` cKeywords  = Token TokKeyword tokStr:lexer (x:xs)
    | tokStr `elem` cTypes     = Token TokType tokStr:lexer (x:xs)
    | otherwise                 = Token TokIdent tokStr:lexer (x:xs)

-- main lexer function
lexer :: String -> [Token]
lexer [] = []
lexer (x:xs)
    | x `elem` cMisc           = Token TokMisc [x]:lexer xs
    | x == '"'                  = lexParseString "" xs
    | x == '\''                 = lexParseChar xs
    | x == '0'                  = if head xs == 'x' then lexParseNum "0x" (tail xs) else lexParseNum "0" xs
    | x `elem` ['1'..'9']       = lexParseNum [x] xs
    | x `elem` cIdentFirst    = lexParseText [x] xs
    | x `elem` cOperatorStart = lexParseOperator x xs
    | otherwise = lexer xs


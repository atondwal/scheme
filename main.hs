import Control.Monad
import Control.Monad.Error
import Control.Applicative ((<$>),(<*>))
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import Data.Complex

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

------------------------------
-- Language representation
------------------------------

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | String String
             | Bool Bool
             | Character Char
             | Ratio Rational
             | Vector [LispVal]
             | Complex (Complex Float)

instance Show LispVal where show = showVal
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

------------------------------
-- Error handling
------------------------------

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where show = showError
showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected 
                                  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

--------------------------------------------------------------------------------
-- Evaluator
--------------------------------------------------------------------------------

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args)
    (lookup func primitives)

------------------------------
-- Built-in functions
------------------------------

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", unaryOp isSymbol),
              ("number?", unaryOp isNumber),
              ("string?", unaryOp isString),
              ("bool?", unaryOp isBool),
              ("list?", unaryOp isList),
              ("symbol2string", unaryOp symbol2string),
              ("string2symbol", unaryOp string2symbol)]

isSymbol :: LispVal -> LispVal
isSymbol (Atom _) = Bool True
isSymbol _ = Bool False

isNumber :: LispVal -> LispVal
isNumber (Number _) = Bool True
isNumber _ = Bool False

isString :: LispVal -> LispVal
isString (String _) = Bool True
isString _ = Bool False

isBool :: LispVal -> LispVal
isBool (Bool _) = Bool True
isBool _ = Bool False

isList :: LispVal -> LispVal
isList (List _) = Bool True
isList (DottedList _ _) = Bool True
isList _ = Bool False

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = return (f v)
unaryOp f (x:_) = throwError $ Default
  "Unary function expected a single argument. Multiple provided."
unaryOp f [] = throwError $ Default "Unary function expected an argument"

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                          if null parsed 
                            then throwError $ TypeMismatch "number" $ String n
                            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s)   = String s
symbol2string _          = String ""
string2symbol (String s) = Atom s
string2symbol _          = Atom ""

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseBool :: Parser LispVal
parseBool = do
  char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseString :: Parser LispVal
parseString = do 
  char '"'
  x <- many (noneOf "\"\\" <|> (char '\\' >> oneOf "nrt\\\""))
  char '"'
  return$ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  return$ case first:rest of 
	  "#t" -> Bool True
	  "#f" -> Bool False
	  _ -> Atom$ first:rest

parseDigital1 :: Parser LispVal
parseDigital1 = many1 digit >>= (return . Number . read)

parseDigital2 :: Parser LispVal
parseDigital2 = do try $ string "#d"
                   x <- many1 digit
                   return (Number (read x))

parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              x <- many1 hexDigit
              return (Number (hex2dig x))

parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              x <- many1 octDigit
              return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              x <- many1 (oneOf "10")
              return $ Number (bin2dig x)

parseCharacter :: Parser LispVal
parseCharacter = do try $ string "#\\"
                    value <- try (string "newline" <|> string "space")
                             <|> do { x <- anyChar; notFollowedBy alphaNum ; return [x] }
                    return $ Character $ case value of
                                         "space" -> ' '
                                         "newline" -> '\n'
                                         otherwise -> (value !! 0)

oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) =
  let old = 2 * digint + (if x == '0' then 0 else 1) in
  bin2dig' old xs

parseNumber :: Parser LispVal
parseNumber =  parseDigital1
           <|> parseDigital2
           <|> parseHex
           <|> parseOct
           <|> parseBin

parseFloat :: Parser LispVal
parseFloat = do i <- many1 digit
                char '.'
                f <- many1 digit
                return $ Float (read (i++'.':f) :: Float)

parseRatio :: Parser LispVal
parseRatio = do n <- many1 digit
                char '/'
                d <- many1 digit
                return $ Ratio (read (n++'%':d) :: Rational)

parseComplex :: Parser LispVal
parseComplex = do Float r <- parseFloat
                  char '/'
                  Float i <- parseFloat
                  return $ Complex ( r :+ i )

parseVector :: Parser LispVal
parseVector = do string "#("
                 vals <- sepBy parseExpr spaces
                 string ")"
                 return $ Vector vals

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
                char '`'
                x <- parseExpr
                return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
                char ','
                x <- parseExpr
                return $ List [Atom "unquote", x]

parseDottedList :: Parser LispVal
parseDottedList = do
  h <- endBy parseExpr spaces
  t <- char '.' >> spaces >> parseExpr
  return$ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted =
  char '\'' >>
  parseExpr >>=
  return . List . ((Atom "quote"):) . (:[])

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> try parseNumber
         <|> try parseBool
         <|> try parseQuoted
         <|> try parseCharacter
         <|> try parseFloat
         <|> try parseRatio
         <|> try parseComplex
         <|> try parseQuasiQuoted
         <|> try parseUnQuote
         <|> try parseVector
         <|> do char '('
                x <- (try parseList) <|> parseDottedList
                char ')'
                return x

--------------------------------------------------------------------------------
-- Misc. Functions
--------------------------------------------------------------------------------

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

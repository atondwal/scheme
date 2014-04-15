import Control.Monad
import Control.Applicative ((<$>),(<*>))
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import Data.Complex

main :: IO ()
main = getArgs >>= putStrLn . readExpr . head 

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left e -> "Parse Error: " ++ show e
  Right _ -> "You Parse!"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | String String
             | Bool Bool
             | Character Char
             | Ratio Rational
             | Complex (Complex Float)

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
         <|> do char '('
                x <- (try parseList) <|> parseDottedList
                char ')'
                return x

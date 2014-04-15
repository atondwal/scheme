import Control.Monad
import Control.Applicative ((<$>),(<*>))
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = getArgs >>= putStrLn . readExpr . head 

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"

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
             | String String
             | Bool Bool

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

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

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
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- (try parseList) <|> parseDottedList
                char ')'
                return x
